/*
 * Copyright 2018-2021 47 Degrees Open Source <https://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package higherkindness.skeuomorph.protobuf

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.os72.protocjar.Protoc
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.{Label, Type}
import com.google.protobuf.DescriptorProtos.FileDescriptorSet
import com.google.protobuf.DescriptorProtos.UninterpretedOption.NamePart
import com.google.protobuf.DescriptorProtos._
import higherkindness.skeuomorph.FileUtils._
import higherkindness.skeuomorph.{Parser, _}

import higherkindness.droste._
import higherkindness.droste.syntax.embed._

import scala.jdk.CollectionConverters._

object ParseProto {

  import ProtobufF._
  import Protocol._

  final case class ProtoSource(
      filename: String,
      path: String,
      importRoot: Option[String] = None
  )

  final case class NamedDescriptor[A](
      scalaPackage: String,
      filePackage: String,
      enclosingProto: String,
      parentMessageNames: List[String],
      name: String,
      desc: A
  ) {

    /*
     * This is the full protobuf name that other protobuf messages will
     * use to reference this type.
     * e.g. .com.acme.Book
     *
     * Note that this is different from the fully-qualified name
     * of the generated Scala class, which will be inside an object
     * with the name of the .proto file,
     * e.g. com.acme.book.Book
     */
    val fullProtoName: String =
      (List(".", filePackage) ++ parentMessageNames :+ name).mkString(".")

    /*
     * The path to the Scala class that corresponds to this type.
     * e.g. com.acme.book
     */
    val scalaPrefix: List[String] =
      (scalaPackage.split('.').toList :+ enclosingProto) ++ parentMessageNames

  }

  implicit def parseProto[F[_], T](implicit T: Embed[ProtobufF, T]): Parser[F, ProtoSource, Protocol[T]] =
    new Parser[F, ProtoSource, Protocol[T]] {
      override def parse(input: ProtoSource)(implicit S: Sync[F]): F[Protocol[T]] =
        runProtoc(input)
    }

  private def runProtoc[F[_]: Sync, T](input: ProtoSource)(implicit T: Embed[ProtobufF, T]): F[Protocol[T]] = {
    val descriptorFileName = s"${input.filename}.desc"

    val protoPaths = input.importRoot
      .foldLeft(Array(s"--proto_path=${input.path}"))((arr, i) => arr :+ s"--proto_path=$i")

    val protoCompilation: F[Int] = Sync[F].delay(
      Protoc.runProtoc(
        protoPaths ++ Array(
          "--plugin=protoc-gen-proto2_to_proto3",
          "--include_imports",
          s"--descriptor_set_out=${input.filename}.desc",
          input.filename
        )
      )
    )

    for {
      _ <- Sync[F].ensure[Int](protoCompilation)(ProtobufCompilationException())((exitCode: Int) => exitCode == 0)
      fileDescriptor <- Sync[F].adaptError(makeFileDescriptor[F](descriptorFileName)) { case ex: Exception =>
        ProtobufParsingException(ex)
      }
      nativeDescriptors <- getTFiles[F, T](input.filename, fileDescriptor)
    } yield nativeDescriptors
  }

  private def makeFileDescriptor[F[_]: Sync](descriptorFileName: String): F[FileDescriptorSet] =
    fileInputStream(descriptorFileName).use(fis => Sync[F].delay(FileDescriptorSet.parseFrom(fis)))

  private def getTFiles[F[_]: Sync, T](descriptorFileName: String, source: FileDescriptorSet)(implicit
      T: Embed[ProtobufF, T]
  ): F[Protocol[T]] = {
    Sync[F].delay {
      fromProto[T](descriptorFileName, source.getFileList.asScala.toList)
    }
  }

  private def scalaPackage(file: FileDescriptorProto): String = {

    val javaPackage: String = file.getOptions.getJavaPackage

    val filePackage: String = file.getPackage

    val fileName: String = formatName(file.getName)
    // This package naming behavior is consistent with ScalaPB, see
    // https://scalapb.github.io/docs/generated-code/#default-package-structure
    if (javaPackage.isEmpty) {
      if (filePackage.isEmpty)
        fileName
      else filePackage
    } else javaPackage

  }

  def fromProto[A](
      descriptorFileName: String,
      files: List[FileDescriptorProto]
  )(implicit A: Embed[ProtobufF, A]): Protocol[A] =
    findDescriptorProto(descriptorFileName, files)
      .map { file =>
        val messages: List[A] = file.getMessageTypeList.asScala.toList.map(d => toMessage[A](d, files))

        val enums: List[A] = file.getEnumTypeList.asScala.toList.map(toEnum[A])

        val fileName: String = formatName(file.getName)

        Protocol[A](
          fileName,
          scalaPackage(file),
          Nil,
          messages ++ enums,
          file.getServiceList.asScala.toList.map(s => toService[A](s, files)),
          imports = Nil
        )
      }
      .getOrElse(throw ProtobufNativeException(s"Could not find descriptors for: $descriptorFileName"))

  def findDescriptorProto(name: String, files: List[FileDescriptorProto]): Option[FileDescriptorProto] =
    files.find(_.getName == name)

  def formatName(name: String): String =
    name
      .split("/")
      .last
      .replace(".proto", "")

  def toService[A](s: ServiceDescriptorProto, files: List[FileDescriptorProto])(implicit
      A: Embed[ProtobufF, A]
  ): Service[A] =
    Service(s.getName, s.getMethodList.asScala.toList.map(o => toOperation[A](o, files)))

  def toOperation[A](o: MethodDescriptorProto, files: List[FileDescriptorProto])(implicit
      A: Embed[ProtobufF, A]
  ): Protocol.Operation[A] =
    Protocol.Operation(
      name = o.getName,
      request = findMessage(o.getInputType, files)
        .fold(`null`[A]())(namedMessage => namedType[A](namedMessage.scalaPrefix, namedMessage.name))
        .embed,
      requestStreaming = o.getClientStreaming,
      response = findMessage(o.getOutputType, files)
        .fold(`null`[A]())(namedMessage => namedType[A](namedMessage.scalaPrefix, namedMessage.name))
        .embed,
      responseStreaming = o.getServerStreaming
    )

  /*
   * Maps are encoded in protobuf as lists of (key, value) pairs, with each
   * pair encoded as if it were an embedded message with two fields, `key` and
   * `value`. A better explanation is available in the protobuf docs:
   * https://developers.google.com/protocol-buffers/docs/proto3#backwards-compatibility
   *
   * The protoc tool generates types for these 'pseudo-message' pairs, but we
   * don't care about them (we don't want to generate code for them) so we want
   * to filter them out when collecting message types.
   */
  def isMapEntryType(descriptor: DescriptorProto): Boolean =
    descriptor.getOptions.getMapEntry

  def toMessage[A](descriptor: DescriptorProto, files: List[FileDescriptorProto])(implicit
      A: Embed[ProtobufF, A]
  ): A = {
    val protoFields: List[FieldDescriptorProto] = descriptor.getFieldList.asScala.toList
    val protoOneOf: List[OneofDescriptorProto]  = descriptor.getOneofDeclList.asScala.toList
    val oneOfFields: List[(FieldF[A], List[Int])] =
      fromOneofDescriptorsProto(protoOneOf, protoFields, descriptor, files)
    val oneOfNumbers: List[Int] = oneOfFields.flatMap(_._2)
    val fields: List[FieldF[A]] =
      protoFields
        .filterNot(f => oneOfNumbers.contains(f.getNumber))
        .map(f => fromFieldDescriptorProto[A](f, descriptor, files))
    val nestedMessages =
      descriptor.getNestedTypeList.asScala.toList
        .filterNot(isMapEntryType)
        .map(toMessage[A](_, files))
    val nestedEnums =
      descriptor.getEnumTypeList.asScala.toList.map(toEnum[A])

    message[A](
      name = descriptor.getName,
      fields = (fields ++ oneOfFields.map(_._1)).sortBy(_.indices.headOption),
      reserved = descriptor.getReservedRangeList.asScala.toList.map(range =>
        (range.getStart until range.getEnd).map(_.toString).toList
      ),
      nestedMessages = nestedMessages,
      nestedEnums = nestedEnums
    ).embed
  }

  def toEnum[A](enum: EnumDescriptorProto)(implicit A: Embed[ProtobufF, A]): A = {
    val (values, aliases) = partitionValuesAliases(enum.getValueList.asScala.toList)

    ProtobufF
      .enum[A](
        name = enum.getName,
        symbols = values,
        options = fromFieldOptionsEnum(enum.getOptions),
        aliases = aliases
      )
      .embed
  }

  def partitionValuesAliases(
      valuesAndAliases: List[EnumValueDescriptorProto]
  ): (List[(String, Int)], List[(String, Int)]) = {
    val (hasAlias, noAlias) = valuesAndAliases.groupBy(_.getNumber).values.partition(_.lengthCompare(1) > 0)
    val separateValueFromAliases: Iterable[(EnumValueDescriptorProto, List[EnumValueDescriptorProto])] = hasAlias.map {
      case h :: t => (h, t)
      case _      => throw ProtobufNativeException(s"Wrong number of aliases")
    }

    (
      (separateValueFromAliases.map(_._1) ++ noAlias.flatten).toList
        .sortBy(_.getNumber)
        .map(i => (i.getName, i.getNumber)),
      separateValueFromAliases.flatMap(_._2).toList.map(i => (i.getName, i.getNumber))
    )
  }

  def fromFieldDescriptorProto[A](
      field: FieldDescriptorProto,
      source: DescriptorProto,
      files: List[FileDescriptorProto]
  )(implicit A: Embed[ProtobufF, A]): FieldF[A] =
    (field.getLabel.isRepeated, isMap(field.getName, source)) match {
      case (true, false) =>
        FieldF.Field(
          name = field.getName,
          position = field.getNumber,
          tpe = repeated[A](fromFieldType(field, files, makeNamedTypesOptional = false)).embed,
          options = fromFieldOptionsMsg(field.getOptions),
          isRepeated = true,
          isMapField = false
        )
      case (_, true) =>
        FieldF.Field(
          name = field.getName,
          position = field.getNumber,
          tpe = getTMap(field.getName, source, files),
          options = fromFieldOptionsMsg(field.getOptions),
          isRepeated = false,
          isMapField = true
        )
      case (_, isMapField) =>
        FieldF.Field(
          name = field.getName,
          position = field.getNumber,
          tpe = fromFieldType(field, files, makeNamedTypesOptional = true),
          options = fromFieldOptionsMsg(field.getOptions),
          isRepeated = field.getLabel.isRepeated,
          isMapField = isMapField
        )
    }

  def isMapEntryTypeForField(desc: DescriptorProto, fieldName: String): Boolean =
    isMapEntryType(desc) &&
      matchNameEntry(fieldName, desc) &&
      containsOnlyMapEntryFields(desc.getFieldList.asScala.toList)

  def isMap(fieldName: String, source: DescriptorProto): Boolean =
    source.getNestedTypeList.asScala.toList.exists(isMapEntryTypeForField(_, fieldName))

  def getTMap[A](name: String, source: DescriptorProto, files: List[FileDescriptorProto])(implicit
      A: Embed[ProtobufF, A]
  ): A =
    (for {
      maybeMsg <-
        source.getNestedTypeList.asScala.toList
          .find(isMapEntryTypeForField(_, name))
      maybeKey   <- getMapField(maybeMsg, "key")
      maybeValue <- getMapField(maybeMsg, "value")
    } yield map(
      fromFieldType(maybeKey, files, makeNamedTypesOptional = false),
      fromFieldType(maybeValue, files, makeNamedTypesOptional = false)
    ).embed)
      .getOrElse(throw ProtobufNativeException(s"Could not find map entry for: $name"))

  def getMapField(msg: DescriptorProto, name: String): Option[FieldDescriptorProto] =
    msg.getFieldList.asScala.toList.find(_.getName == name)

  def containsOnlyMapEntryFields(fields: List[FieldDescriptorProto]): Boolean =
    fields.count(f => (f.getNumber == 1 && f.getName == "key") || (f.getNumber == 2 && f.getName == "value")) == 2

  def matchNameEntry(name: String, source: DescriptorProto): Boolean =
    normalizeName(source.getName) == normalizeName(s"${name}Entry")

  def normalizeName(name: String): String =
    name.toLowerCase.replace("_", "")

  def fromOneofDescriptorsProto[A](
      oneOfFields: List[OneofDescriptorProto],
      fields: List[FieldDescriptorProto],
      source: DescriptorProto,
      files: List[FileDescriptorProto]
  )(implicit
      A: Embed[ProtobufF, A]
  ): List[(FieldF[A], List[Int])] =
    oneOfFields.zipWithIndex.map { case (oneof, index) =>
      val oneOfFields: NonEmptyList[FieldF.Field[A]] = NonEmptyList
        .fromList(
          fields
            .filter(t => t.hasOneofIndex && t.getOneofIndex == index)
            .map(fromFieldDescriptorProto(_, source, files))
            .collect { case b @ FieldF.Field(_, _, _, _, _, _) => b }
        )
        .getOrElse(throw ProtobufNativeException(s"Empty set of fields in OneOf: ${oneof.getName}"))

      val fOneOf  = oneOf(name = oneof.getName, fields = oneOfFields)
      val indices = oneOfFields.map(_.position).toList
      (FieldF.OneOfField(name = oneof.getName, tpe = fOneOf.embed, indices), indices)
    }

  def fromFieldTypeCoalgebra(
      field: FieldDescriptorProto,
      files: List[FileDescriptorProto],
      makeNamedTypesOptional: Boolean
  ): Coalgebra[ProtobufF, Type] =
    Coalgebra {
      case Type.TYPE_BOOL     => TBool()
      case Type.TYPE_BYTES    => TBytes()
      case Type.TYPE_DOUBLE   => TDouble()
      case Type.TYPE_FIXED32  => TFixed32()
      case Type.TYPE_FIXED64  => TFixed64()
      case Type.TYPE_FLOAT    => TFloat()
      case Type.TYPE_INT32    => TInt32()
      case Type.TYPE_INT64    => TInt64()
      case Type.TYPE_SFIXED32 => TSfixed32()
      case Type.TYPE_SFIXED64 => TSfixed64()
      case Type.TYPE_SINT32   => TSint32()
      case Type.TYPE_SINT64   => TSint64()
      case Type.TYPE_STRING   => TString()
      case Type.TYPE_UINT32   => TUint32()
      case Type.TYPE_UINT64   => TUint64()
      case Type.TYPE_ENUM =>
        findEnum(field.getTypeName, files)
          .fold[ProtobufF[Type]](TNull()) { namedEnum =>
            if (makeNamedTypesOptional)
              TOptionalNamedType(namedEnum.scalaPrefix, namedEnum.name)
            else
              TNamedType(namedEnum.scalaPrefix, namedEnum.name)
          }
      case Type.TYPE_MESSAGE =>
        findMessage(field.getTypeName, files)
          .fold[ProtobufF[Type]](TNull()) { namedMessage =>
            if (makeNamedTypesOptional)
              TOptionalNamedType(namedMessage.scalaPrefix, namedMessage.name)
            else
              TNamedType(namedMessage.scalaPrefix, namedMessage.name)
          }
      case _ => TNull()
    }

  def fromFieldType[A](field: FieldDescriptorProto, files: List[FileDescriptorProto], makeNamedTypesOptional: Boolean)(
      implicit A: Embed[ProtobufF, A]
  ): A =
    scheme.ana(fromFieldTypeCoalgebra(field, files, makeNamedTypesOptional)).apply(field.getType)

  def fromFieldOptionsMsg(options: FieldOptions): List[OptionValue] =
    OptionValue("deprecated", options.getDeprecated.toString) ::
      options.getUninterpretedOptionList.asScala.toList.map(t =>
        OptionValue(toString(t.getNameList.asScala.toList), t.getIdentifierValue)
      )

  def fromFieldOptionsEnum(options: EnumOptions): List[OptionValue] = {
    List(
      OptionValue("allow_alias", options.getAllowAlias.toString),
      OptionValue("deprecated", options.getDeprecated.toString)
    ) ++
      options.getUninterpretedOptionList.asScala.toList.map(t =>
        OptionValue(toString(t.getNameList.asScala.toList), t.getIdentifierValue)
      )
  }

  def toString(nameParts: Seq[NamePart]): String =
    nameParts.foldLeft("")((l, r) => if (r.getIsExtension) s"$l.($r)" else s"$l.$r")

  def allMessages(files: List[FileDescriptorProto]): List[NamedDescriptor[DescriptorProto]] = {
    files.flatMap { f =>
      val enclosingProto = formatName(f.getName)

      def rec(m: DescriptorProto, parents: List[String]): List[NamedDescriptor[DescriptorProto]] =
        NamedDescriptor(
          scalaPackage(f),
          f.getPackage,
          enclosingProto,
          parents,
          m.getName,
          m
        ) :: m.getNestedTypeList.asScala.toList
          .filterNot(isMapEntryType)
          .flatMap(rec(_, parents :+ m.getName))

      f.getMessageTypeList.asScala.toList.flatMap(rec(_, Nil))
    }
  }

  def findMessage(name: String, files: List[FileDescriptorProto]): Option[NamedDescriptor[DescriptorProto]] =
    allMessages(files).find(_.fullProtoName.endsWith(name))

  def findEnum(name: String, files: List[FileDescriptorProto]): Option[NamedDescriptor[EnumDescriptorProto]] = {
    val allTopLevel: List[NamedDescriptor[EnumDescriptorProto]] = files.flatMap { f =>
      val enclosingProto = formatName(f.getName)
      f.getEnumTypeList.asScala.toList.map(e =>
        NamedDescriptor(scalaPackage(f), f.getPackage, enclosingProto, Nil, e.getName, e)
      )
    }

    val allNestedInsideMessages = for {
      m <- allMessages(files)
      e <- m.desc.getEnumTypeList.asScala
    } yield {
      val parentMessageNames = m.parentMessageNames :+ m.name
      m.copy(parentMessageNames = parentMessageNames, name = e.getName, desc = e)
    }
    (allTopLevel ++ allNestedInsideMessages).find(_.fullProtoName.endsWith(name))
  }

  implicit class LabelOps(self: Label) {
    def isRepeated: Boolean = self.name() == "LABEL_REPEATED"
  }
}
