#include <stdio.h>

typedef struct Book
{
    int id;
    char  title[100];
    char autor[100];
    int publication_year;
    // Todo: agregra genero del libro
    int quantity;
} book_t;

typedef struct Member
{
    int id;
    char name[100];
    int bookIssued; // TODO: Hacer una lista de libros prestados
} member_t;

void addBook()
{
    printf("Anadiendo un libro\n");
}

void displayBook()
{
    printf("Mostrando libros disponibles en la biblioteca\n");
}

void addMember()
{
    printf("Anadiendo un miembro\n");
}

void issueBook()
{
    printf("Prestando un libro\n");
}

int main()
{
    int choice = 0;
    do
    {
        printf("\nMenu de sistema de manejo de biblioteca\n");
        printf("\t1. Agregar un libros\n");
        printf("\t2. Mostar libros disponibles\n");
        printf("\t3. Agregar un miembro\n");
        printf("\t4. Prestar libro\n");
        printf("\t5. Salir\n");
        printf("Indica tu opcion\n");
        scanf("%d", &choice);

        switch (choice)
        {
        case 1:
        //addBook();
            break;

        case 2:
        //displayBook();
            break;

        case 3:
        //addMember();
            break;

        case 4:
        //issueBook();
            break;

        default:
        printf("Esta opcion no es valida\n");
            break;
        }
    } while (choice != 5);
    return 0;
}