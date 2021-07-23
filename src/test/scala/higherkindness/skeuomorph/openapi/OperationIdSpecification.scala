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

package higherkindness.skeuomorph.openapi

import higherkindness.skeuomorph.openapi.client.print._
import helpers._

class OperationIdSpecification extends org.specs2.mutable.Specification {

  import cats.syntax.all._
  import OperationIdSpecification._
  import Http._

  "operation id should able normalize" >> {

    "when a description is provided" >> {
      OperationId(
        Verb.Put,
        Path("/foo"),
        defaultOperation.withOperationId("createPayload")
      ).show must ===("createPayload")
    }

    "when a description is not provided" >> {
      OperationId(
        Verb.Put,
        Path("/foo"),
        defaultOperation
      ).show must ===("updateFoo")
    }

    "when the description contains special characters" >> {
      OperationId(
        Verb.Put,
        Path("/foo"),
        defaultOperation.withOperationId("create-Payload_V1")
      ).show must ===("`create-Payload_V1`")
    }

    "when the description is not provided and contains special characters" >> {
      OperationId(
        Verb.Get,
        Path("/v1/pet-details_eager"),
        defaultOperation
      ).show must ===("getPetDetailsEagerV1")
    }

    "when the path contains path variables" >> {
      OperationId(
        Verb.Delete,
        Path("/pets/{petId}/owners"),
        defaultOperation
      ).show must ===("deleteOwnersPetsByPetId")
    }

    "when the path contains query params" >> {
      OperationId(
        Verb.Delete,
        Path("/pets/{petId}/owners?userId={userId}&name={name}"),
        defaultOperation
      ).show must ===("deleteOwnersPetsByPetIdUserIdName")
    }

    "when a description is provided with numbers at the beginning" >> {
      OperationId(
        Verb.Put,
        Path("/foo"),
        defaultOperation.withOperationId("1111createPayload")
      ).show must ===("`1111createPayload`")
    }
  }
}
object OperationIdSpecification {
  import JsonSchemaF.Fixed
  val defaultOperation = operation[JsonSchemaF.Fixed](
    request("application/json" -> mediaType(Fixed.reference("#/components/schemas/Foo"))),
    responses = "201"          -> response("Null response")
  )
}
