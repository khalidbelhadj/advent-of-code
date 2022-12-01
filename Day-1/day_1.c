#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[])
{
    FILE *file = fopen("input.txt", "r");
    int i = 0;
    fscanf(file, "%c", &i);
    while (!feof(file))
    {
        printf("%d\n", i);
        fscanf(file, "%d", &i);
    }
    fclose(file);

    return 0;
}
