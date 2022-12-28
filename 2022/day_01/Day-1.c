#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    FILE *file = fopen("input.txt", "r");

    if (file == NULL)
    {
        printf("File was not read\n");
        return 1;
    }

    char buf[256];
    int top_3[3];
    int sum, max;

    while (fgets(buf, sizeof(buf), file))
    {
        if (buf[0] != '\n')
        {
            sum += atoi(buf);
            continue;
        }
        for (int i = 0; i < 3; i++)
        {
            max = sum >= max ? sum : max;
            if (sum >= top_3[i])
            {
                top_3[i] = sum;
                break;
            }
        }
        sum = 0;
    }
    printf("Part 1: %d\n", max);
    printf("Part 2: %d\n", top_3[0] + top_3[1] + top_3[2]);
    fclose(file);

    return 0;
}
