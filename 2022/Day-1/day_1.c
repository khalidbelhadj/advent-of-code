#include <stdio.h>

int string_to_int(char str[])
{
    int sum = 0;
    int i = 1;
    char ch = str[0]; 
    while(ch != '\0' && ch != '\n')
    {
        sum = 10*sum + (int)(ch - '0');
        ch = str[i];
        i++;
    }
    return sum;
}

int main(int argc, char *argv[])
{
    FILE *file = fopen("input.txt", "r");

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
        else
            sum += string_to_int(buf);
    }
    int temp = 0;
    for(int i = 0; i < 3; i++)
    {
        temp += max[i];
    }
    printf("Sum of top 3 values is %d\n", temp);
    fclose(file);

    return 0;
}
