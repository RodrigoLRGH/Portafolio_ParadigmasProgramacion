---
draft: false
---
# Practica 2: Programacion orientado a objetos

## **Introducción**

Para esta practica el profesor proporciono un codigo en Python para gestionar una blibloteca, con el objetivo de identificar los elementos fundamentales de la programacion orientada a objetos.

### **Clases**

En el codigo contamos con las siguientes clases:

```Python
class Genre:
class Book:
class DigitaBook(Book):
class Member:
class Library:
```

### **Atributos**

Cada una de las clases anteriormente mencionadas tinen su atributos y metodos

#### - Genre

Este cuenta con atributos con constantes de clase, y un metodo para retornar una lista de todos los generos disponibles

```Python
class Genre:
    FICTION = "Ficcion"
    NON_FICTION = "No Ficcion"
    SCIENCE = "Ciencia"
    HISTORY = "Historia"
    FANTASY = "Fantasia"
    BIOGRAPHY = "Biografia"
    OTHER = "Otro"

    @classmethod
    def all_genres(cls):
```

#### - Book

Esta clase cuenta con atributos los cuales representna las caracteristicas de un libro y con varios metodos, uno para construir, otro para destruir, convertir datos del libro a un diccionario y otro para crear un objeto Book a partir de un diccionario

```Python
class Book:
    book_id
    title
    author
    publication_year
    genre
    quantity

    def __init__(self, book_id, title, author, publication_year, genre, quantity):
    def __del__(self):
    def to_dict(self):
    @staticmethod
    def from_dict(data):
```

#### - Digital Book

En esta clase contamos con los mismos atributos de la clase Book, pero adicionalmente tenemos file_format. Ademas los mismos metodos, en el metodo to_dict se incluye el atributo file_format

```Python
class DigitalBook(Book):
    def __init__(self, book_id, title, author, publication_year, genre, quantity, file_format):
    def to_dict(self):
    @staticmethod
    def from_dict(data):
```

#### - Member

En esta clase cuenta con pocos atributos y varios metodos similares al de las otras clases

```Python
class Member:
    member_id
    name
    issued_books = []

    def __init__(self, member_id, name):
    def __del__(self):
    def to_dict(self):
    @staticmethod
    def from_dict(data):
```

#### - Library

Esta clase tiene como atributos una lista de libros y una lista de miembros, y funciones similares a las anteriores ademas de muchas otras funciones

```Python
class Library:
    books = []
    members = []
    __init__(self):
    __del__(self):
    def add_book(self, book):
    find_book_by_id(self, book_id):
    def display_books(self):
    def add_member(self, member):
    def issue_book(self, book_id, member_id):
    def return_book(self, book_id, member_id):
    def find_member_by_id(self, member_id):
    def display_members(self):
    def search_member(self, member_id):
    def save_library_to_file(self, filename):
    def load_library_from_file(self, filename):
    def save_members_to_file(self, filename):
    def load_members_from_file(self, filename):
```

### **Herencia**

En este codigo se puede observar como DigitalBook hereda los atributos y metodos de Book, agregando nuevas funcionalidaes para el atributo file_format, el cual no contiene la clase Book

```Python
class DigitalBook(Book):
    def __init__(self, book_id, title, author, publication_year, genre, quantity, file_format):
        super().__init__(book_id, title, author, publication_year, genre, quantity)
        self.file_format = file_format

def to_dict(self):
    data = super().to_dict()
    data["file_format"] = self.file_format
    return data

```

### **Polimorfismo**

En el codigo se puede observar como se utilizan metodos con el mismo nombre en diferentes clases, como es el caso en Book, DigitalBook y member con el metodo to_dict

```Python
class Book:
    def to_dict(self):
        return {
            "id": self.id,
            "title": self.title,
            "author": self.author,
            "publication_year": self.publication_year,
            "genre": self.genre,
            "quantity": self.quantity
        }

class DigitalBook(Book):
    def to_dict(self):
        data = super().to_dict()
        data["file_format"] = self.file_format
        return data

class Member:
    def to_dict(self):
        return {
            "id": self.id,
            "name": self.name,
            "issued_books": self.issued_books
        }
```

### Objetos

En el menú que se encuentra en la función main, podemos observar cómo, dependiendo de si se quiere agregar un libro, un libro digital o un miembro, se solicitan los datos correspondientes y se almacenan en variables. Estas variables, según la clase que se vaya a instanciar, se utilizan para crear un objeto.

En el caso de agregar un libro:

```Python
if choice == 1:
            book_id = int(input("Ingresa ID del libro: "))
            title = input("Ingresa titulo del libro: ")
            author = input("Ingresa nombre del autor: ")
            publication_year = int(input("Ingresa el ano de publicacion: "))
            genre = input("Ingresa el genero del libro: ")
            quantity = int(input("Ingresa la cantidad de libros: "))
            is_digital = input("Es un libro digital? (s/n): ").lower() == 's'
            if is_digital:
                file_format = input("Ingresa el formato del archivo: ")
                book = DigitalBook(book_id, title, author, publication_year, genre, quantity, file_format)
            else:
                book = Book(book_id, title, author, publication_year, genre, quantity)
            library.add_book(book)

```

En el caso de agregar un miembro:

```Python
elif choice == 3:
    member_id = int(input("Ingresa el ID del miembro: "))
    name = input("Ingresa el nombre del miembro: ")
    member = Member(member_id, name)
    library.add_member(member)
```