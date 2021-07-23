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

import io.circe.Decoder
import JsonDecoders._

import schema._
import cats.syntax.all._
import JsonSchemaF.Fixed

class OpenApiDecoderSpecification extends org.specs2.mutable.Specification {
  import helpers._

  "Info object should be able to decode" >> {
    "when is valid" >> {
      val json = unsafeParse("""{
            "title": "Sample Pet Store App",
            "description": "This is a sample server for a pet store.",
            "termsOfService": "http://example.com/terms/",
            "contact": {
              "name": "API Support",
              "url": "http://www.example.com/support",
              "email": "support@example.com"
            },
            "license": {
              "name": "Apache 2.0",
              "url": "https://www.apache.org/licenses/LICENSE-2.0.html"
            },
            "version": "1.0.1"
          }""")

      Decoder[Info].decodeJson(json) must beRight(
        Info("Sample Pet Store App", "This is a sample server for a pet store.".some, "1.0.1")
      )
    }
  }

  "Header object should be able to decode" >> {
    "when a valid object is provided" >> {
      val json = unsafeParse("""
      {
        "description": "The number of allowed requests in the current period",
        "schema": {
          "type": "integer"
        }
      }
      """)
      Decoder[Header[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        Header[JsonSchemaF.Fixed]("The number of allowed requests in the current period", Fixed.integer())
      )
    }

  }

  "Tag object should be able to decode" >> {
    "when a valid object is provided" >> {
      val json = unsafeParse("""
      {
        "name": "pet",
        "description": "Pets operations"
      }
      """)
      Decoder[Tag].decodeJson(json) must beRight(Tag("pet", "Pets operations".some, None))
    }

  }

  "External Documentation object should be able to decode" >> {
    "when a valid object is provided" >> {
      val json = unsafeParse("""
      {
        "description": "Find more info here",
        "url": "https://example.com"
      }
      """)
      Decoder[ExternalDocs].decodeJson(json) must beRight(
        ExternalDocs("https://example.com", "Find more info here".some)
      )
    }

  }

  "Reference object should be able to decode" >> {
    "when a valid object is provided" >> {
      val json = unsafeParse(s"""
      {
        "$$ref": "#/components/schemas/Pet"
      }
      """)

      Decoder[Reference].decodeJson(json) must beRight(Reference("#/components/schemas/Pet"))
    }
  }

  "Callback object should be able to decode" >> {
    "when a valid object is provided" >> {
      val json =
        unsafeParse(s"""
      {
          "http://notificationServer.com?transactionId={$$request.body#/id}&email={$$request.body#/email}": {
            "post": {
              "requestBody": {
                "description": "Callback payload",
                "content": {
                  "application/json": {
                    "schema": {
                      "$$ref": "#/components/schemas/SomePayload"
                    }
                  }
                }
              },
              "responses": {
                "200": {
                  "description": "webhook successfully processed and no retries will be performed"
                }
              }
          }
        }
      }
      """)

      val itemObject = emptyItemObject[JsonSchemaF.Fixed]
        .withPost(
          operation(
            request("application/json" -> mediaType(Fixed.reference("#/components/schemas/SomePayload"))).optional
              .withDescription("Callback payload"),
            responses = "200" -> response("webhook successfully processed and no retries will be performed")
          )
        )

      Decoder[Callback[JsonSchemaF.Fixed]].decodeJson(json) should beRight(
        Callback(
          "http://notificationServer.com?transactionId={$request.body#/id}&email={$request.body#/email}" ->
            itemObject
        )
      )
    }
  }

  "Components object should be able to decode" >> {
    "when an empty object is provided" >> {
      val json = unsafeParse("{}")
      Decoder[Components[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        components[JsonSchemaF.Fixed]()
      )
    }
    "when parameters are provided" >> {
      val json = unsafeParse(""" 
      {
        "parameters": {
          "skipParam": {
            "name": "skip",
            "in": "query",
            "description": "number of items to skip",
            "required": true,
            "schema": {
              "type": "integer",
              "format": "int32"
            }
          },
          "limitParam": {
            "name": "limit",
            "in": "query",
            "description": "max records to return",
            "required": true,
            "schema" : {
              "type": "integer",
              "format": "int32"
            }
          }
        }
      }
      """)
      Decoder[Components[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        components[JsonSchemaF.Fixed]().copy(parameters =
          Map(
            "skipParam" -> query(
              "skip",
              JsonSchemaF.Fixed.integer(),
              required = true,
              description = "number of items to skip".some
            ).asLeft,
            "limitParam" -> query(
              "limit",
              JsonSchemaF.Fixed.integer(),
              required = true,
              description = "max records to return".some
            ).asLeft
          )
        )
      )

    }
    "when an schemas are provided" >> {
      val json = unsafeParse(""" 
      {
        "schemas": {
          "GeneralError": {
            "type": "object",
            "properties": {
              "code": {
                "type": "integer",
                "format": "int32"
              },
              "message": {
                "type": "string"
              }
            }
          },
          "Category": {
            "type": "object",
            "properties": {
              "id": {
                "type": "integer",
                "format": "int64"
              },
              "name": {
                "type": "string"
              }
            }
          }
        }
      }
      """)
      Decoder[Components[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        components[JsonSchemaF.Fixed](
          "GeneralError" -> obj("code" -> Fixed.integer(), "message" -> Fixed.string())(),
          "Category"     -> obj("id" -> Fixed.long(), "name" -> Fixed.string())()
        )
      )
    }
  }

  "Encoding object should be able to decode" >> {
    "when a valid object is provided" >> {
      val json = unsafeParse("{}")
      Decoder[Encoding[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        Encoding[JsonSchemaF.Fixed](None, Map.empty, None, None, None)
      )
    }
  }

  "Server object should be able to decode" >> {
    "when is valid and variables are not provided" >> {
      val json = unsafeParse(
        """{
          "url": "https://development.gigantic-server.com/v1",
          "description": "Development server"
        }"""
      )

      Decoder[Server].decodeJson(json) must beRight(
        Server("https://development.gigantic-server.com/v1", "Development server".some, Map.empty)
      )
    }

    "when is valid and variables are provided" >> {
      val json = unsafeParse(
        """{
          "url": "https://{username}.gigantic-server.com:{port}/{basePath}",
          "description": "The production API server",
          "variables": {
            "username": {
              "default": "demo",
              "description": "this value is assigned by the service provider, in this example `gigantic-server.com`"
            },
            "port": {
              "enum": [
                "8443",
                "443"
              ],
              "default": "8443"
            },
            "basePath": {
              "default": "v2"
            }
          }
        }"""
      )

      Decoder[Server].decodeJson(json) must beRight(
        Server(
          "https://{username}.gigantic-server.com:{port}/{basePath}",
          "The production API server".some,
          Map(
            "username" -> Server.Variable(
              List.empty,
              "demo",
              """this value is assigned by the service provider, in this example `gigantic-server.com`""".some
            ),
            "port"     -> Server.Variable(List("8443", "443"), "8443", None),
            "basePath" -> Server.Variable(List.empty, "v2", None)
          )
        )
      )
    }
  }
  "Open api object should be able to decode" >> {
    "when required fields are provided" >> {
      val json = unsafeParse("""
      {
        "openapi" : "3.0.0",
        "info" : {
          "title": "Swagger Petstore",
          "version": "1.0.0"
        }
      }
      """)

      Decoder[OpenApi[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        OpenApi[JsonSchemaF.Fixed](
          "3.0.0",
          Info("Swagger Petstore", None, "1.0.0"),
          List.empty,
          Map.empty,
          None,
          List.empty,
          None
        )
      )
    }

    "when paths are provided" >> {
      val json = unsafeParse("""
      {
        "openapi" : "3.0.0",
        "info" : {
          "title": "Swagger Petstore",
          "version": "1.0.0"
        },
        "paths": {
          "/pets": {
          }
        }
      }
      """)

      Decoder[OpenApi[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        OpenApi[JsonSchemaF.Fixed](
          "3.0.0",
          Info("Swagger Petstore", None, "1.0.0"),
          List.empty,
          Map("/pets" -> emptyItemObject[JsonSchemaF.Fixed]),
          None,
          List.empty,
          None
        )
      )
    }
  }

  "Parameter object should be able to decode" >> {
    "when path object is provided" >> {
      val json = unsafeParse("""
      {
        "name": "username",
        "in": "path",
        "required": true,
        "schema": {
          "type": "string"
        }
      }
      """)
      Decoder[Parameter[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        path(name = "username", schema = Fixed.string())
      )
    }

    "when query object is provided" >> {
      val json = unsafeParse("""
      {
        "name": "limit",
        "in": "query",
        "description": "How many items to return at one time (max 100)",
        "required": false,
        "schema": {
          "type": "integer",
          "format": "int32"
        }
      }
      """)
      Decoder[Parameter[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        query(
          name = "limit",
          description = "How many items to return at one time (max 100)".some,
          allowEmptyValue = false,
          schema = Fixed.integer()
        )
      )
    }

    "when header object is provided" >> {
      val json = unsafeParse("""
      {
        "name": "token",
        "in": "header",
        "description": "token to be passed as a header",
        "required": true,
        "schema": {
          "type": "array",
          "items": {
            "type": "integer",
            "format": "int64"
          }
        },
        "style": "simple"
      }
      """)

      Decoder[Parameter[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        Parameter.Header(
          name = "token",
          description = "token to be passed as a header".some,
          required = true,
          style = "simple",
          schema = Fixed.array(Fixed.long())
        )
      )
    }

    "when cookie object is provided" >> {
      val json = unsafeParse("""{
        "in": "cookie",
        "name": "csrftoken",
        "schema": {
          "type": "string"
        }
      }""")

      Decoder[Parameter[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        Parameter.Cookie(
          name = "csrftoken",
          description = None,
          schema = Fixed.string()
        )
      )
    }

  }

  "Path item object should be able to decode" >> {
    "when only parameters are provided" >> {
      val json = unsafeParse("""{
        "parameters": [
          {
            "name": "id",
            "in": "path",
            "description": "ID of pet to use",
            "required": true,
            "schema": {
              "type": "array",
              "items": {
                "type": "string"
              }
            },
            "style": "simple"
          }
        ]
      }""")

      Decoder[Path.ItemObject[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        emptyItemObject[JsonSchemaF.Fixed].withParameter(
          path("id", JsonSchemaF.Fixed.array(JsonSchemaF.Fixed.string()), description = "ID of pet to use".some)
        )
      )
    }

    "when get operation is defined" >> {
      val json = unsafeParse("""
      {
        "post": 
      {
        "tags": [
          "pet"
        ],
        "summary": "Updates a pet in the store with form data",
        "operationId": "updatePetWithForm",
        "parameters": [
          {
            "name": "petId",
            "in": "path",
            "description": "ID of pet that needs to be updated",
            "required": true,
            "schema": {
              "type": "string"
            }
          }
        ],
        "requestBody": {
          "content": {
            "application/x-www-form-urlencoded": {
              "schema": {
                "type": "object",
                 "properties": {
                    "name": { 
                      "description": "Updated name of the pet",
                      "type": "string"
                    },
                    "status": {
                      "description": "Updated status of the pet",
                      "type": "string"
                   }
                 },
              "required": ["status"] 
              }
            }
          }
        },
        "responses": {
          "200": {
            "description": "Pet updated.",
            "content": {
              "application/json": {},
              "application/xml": {}
            }
          },
          "405": {
            "description": "Method Not Allowed",
            "content": {
              "application/json": {},
              "application/xml": {}
            }
          }
        },
        "security": [
          {
            "petstore_auth": [
              "write:pets",
              "read:pets"
            ]
          }
        ]
      }
    }
      """)

      val expectedResponseContent = List(
        "application/json" -> noneMediaType[JsonSchemaF.Fixed],
        "application/xml"  -> noneMediaType[JsonSchemaF.Fixed]
      )

      val expectedOperation =
        operation[JsonSchemaF.Fixed](
          request(
            content = "application/x-www-form-urlencoded" ->
              mediaType(
                obj(
                  "name"   -> Fixed.string(),
                  "status" -> Fixed.string()
                )("status")
              )
          ).optional,
          "200" -> response("Pet updated.", expectedResponseContent: _*),
          "405" -> response("Method Not Allowed", expectedResponseContent: _*)
        )
      Decoder[Path.ItemObject[JsonSchemaF.Fixed]].decodeJson(json) must beRight(
        emptyItemObject[JsonSchemaF.Fixed].withPost(
          expectedOperation
            .withParameter(path("petId", JsonSchemaF.Fixed.string(), "ID of pet that needs to be updated".some))
            .withOperationId("updatePetWithForm")
            .withTag("pet")
            .withSummary("Updates a pet in the store with form data")
        )
      )
    }
  }
}
