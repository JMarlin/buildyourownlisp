#include <stdio.h>

//Chapter 3 bonus 3
void print_n_hellos(int n) {

    if(!n)
        return;

    puts("Hello, world!");
    print_n_hellos(n - 1);
}

//int shite(int argc, char** argv) { //Chapter 2 bonus 2
int main(int argc, char** argv) { //Chapter 2 bonus 2

    int i;

    for(i = 0; i < 5; i++)       //Chapter 3 bonus 1
        puts("Whuttup, betch?"); //Chapter 2 bonus 1

    //Chapter 3 bonus 2
    while(i--)
        puts("Hello, world!");

    //Chapter 3 bonus 3
    print_n_hellos(5);

    return 0;
}
