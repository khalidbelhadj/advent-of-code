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
    int sum = 0;
    int max[] = {0, 0, 0};

    while(fgets(buf, sizeof(buf), file))
    {
        if (buf[0] == '\n')
        {
            for(int i = 0; i < 3; i++)
            {
                if (sum >= max[i])
                {
                    max[i] = sum;
                    break;
                }
            }
            sum = 0;
        }
        else sum += atoi(buf);
    }
    sum = 0;
    for(int i = 0; i < 3; i++)
    {
        sum += max[i];
    }
    printf("Sum of top 3 values is %d\n", sum);
    fclose(file);

    return 0;
}
