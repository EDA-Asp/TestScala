package UsingAndImplicits.TypeClasses1



//define domain models

//opaque type houseNumber = Int


object DomainModels {

  
  opaque type houseNumber = Int

  object houseNumber {
    def apply(value: Int): houseNumber = value
  }
  
  case class Address(street: String, houseNumber: houseNumber, city: String)

  object Address:
    given AddressSerialization: MyJsonSerializing[Address] with
      import ToJsonMethods.*

      def serialize(a: Address): String = {
        s"""|{
            | "street": ${a.street.AsJson},
            | "houseNumber": ${a.houseNumber.AsJson},
            | "city": ${a.city.AsJson}
            |}
            |""".stripMargin
      }

  case class Phone(
                    countryCode: Int,
                    phoneNumber: Long
                  )

  object Phone:
    given PhoneSerialization: MyJsonSerializing[Phone] with
      import ToJsonMethods.*

      override def serialize(o: Phone): String =
        s"""|{
            | "countryCode": ${o.countryCode.AsJson},
            | "phoneNumber": ${o.phoneNumber.AsJson}
            |}
            |""".stripMargin

  case class Contact(name: String, addresses: List[Address], phones: List[Phone])

  object Contact:
    given ContactSerialization: MyJsonSerializing[Contact] with
      import ToJsonMethods.*

      override def serialize(o: Contact): String =
        s"""|{
            | "name": ${o.name.AsJson},
            | "addresses": ${o.addresses.AsJson},
            | "phones": ${o.phones.AsJson}
            |}
            |""".stripMargin

  case class ContactBook(contacts: List[Contact])

  object ContactBook:
    given addressBookSerializer: MyJsonSerializing[ContactBook] with
      def serialize(a: ContactBook): String =
        import ToJsonMethods.*
        s"""|{
            | "contacts": ${a.contacts.AsJson}
            |}""".stripMargin


  trait MyJsonSerializing[T]:

    def serialize(o: T): String
  //extension (o: T) def ToJson: String = serialize(o)

  object MyJsonSerializing:

    import ToJsonMethods.*

    given IntSerialization: MyJsonSerializing[Int] with
      def serialize(o: Int): String = o.toString

    given LongSerialization: MyJsonSerializing[Long] with
      def serialize(o: Long): String = o.toString

    given StringSerialization: MyJsonSerializing[String] with
      def serialize(o: String): String = s"\"$o\""

    given ListSerialization[T: MyJsonSerializing]: MyJsonSerializing[List[T]] with
      def serialize(o: List[T]): String = s"[${o.map(x => x.AsJson).mkString(",")}]"

  object ToJsonMethods:
    extension [T: MyJsonSerializing as serializer](a: T)
      def AsJson: String = serializer.serialize(a)


}



@main
def main(): Unit = {

//  println(List(1, 2, 3).AsJson)
//
//  println(List(1, 2, 3))

  import DomainModels.ToJsonMethods.*
  import DomainModels.*
  

  val addressBook = ContactBook(
      List(
        Contact(
          "Bob Smith",
          List(
            Address(
              "12345 Main Street",
              houseNumber(94105),
              "MMM"
            ),
            Address(
              "500 State Street",
              houseNumber(90007),
              "Los Angeles",
            )
          ),
          List(
            Phone(
              1,
              5558881234L
            ),
            Phone(
              49,
              5558413323L
            )
          )
        )
      )
    )

  println(addressBook.AsJson)


  }
