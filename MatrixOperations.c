#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#define true 1

//determinant function
float det(float **detarray, int detarrsize) {

    //total determinant
    float total = 0;

    //if detarrsize is just a single element, take it.
    if (detarrsize == 1) {
        return detarray[0][0];
    }

    //otherwise, create minors of it
    else {

        //top row minor
        for (int n = 0; n < detarrsize; n++) {

            //variables
            int i2 = 0, j2 = 0;
            float **arr2;
            float factor = detarray[0][n];
            int smallerdetarrsize = detarrsize - 1;

            //create second smaller array
            arr2 = (float **)malloc((smallerdetarrsize) * sizeof(float *));

            for (int i = 0; i < smallerdetarrsize; i++) {
                arr2[i] = (float *)malloc((smallerdetarrsize) * sizeof(float));
                if (arr2[i] == NULL) {
                    printf("ERROR: Memory allocation failure\n");
                    return 0;
                }
            }

            //scanning loop
            for (int i = 0; i < detarrsize; i++) {
                j2 = 0;
                if(i != 0) {
                    for (int j = 0; j < detarrsize; j++) {
                        if (j != n) {

                            //put number from bigger array to smaller array
                            arr2[i2][j2] = detarray[i][j];
                            j2++;
                        }
                    }
                    i2++;
                }
            }

            //sign incorporation
            if ((n+1+1) % 2 == 0) {
                total += (factor * det(arr2, smallerdetarrsize));
            }
            else {
                total -= (factor * det(arr2, smallerdetarrsize));
            }

            //free smaller array from memory now
            free(arr2);
        }
        return total;
    }
}

//transpose function
void transpose(float **transarray, int transarrsize) {
    for (int i = 0; i < transarrsize; i++) {
        for (int j = 0; j < transarrsize; j++) {

            //transpose only one side, the other changes by itself
            if (j > i) {

                //swapping algorithm
                int t = transarray[i][j];
                transarray[i][j] = transarray[j][i];
                transarray[j][i] = t;
            }
        }
    }
}

//Inversion function
float ** invert(float **invarray, int invarrsize) {

    //variables
    float detmat = det(invarray, invarrsize);
    int miniarrsize = invarrsize - 1;

    //create two new array, array2 is the return array
    //and arrnull is the null fallback array
    float **array2;
    float **arrnull;

    //arrnull is initialized and set to be a zero matrix
    arrnull = (float **)malloc(invarrsize * sizeof(float *));

    for (int i = 0; i < invarrsize; i++) {
        arrnull[i] = (float *)malloc(invarrsize * sizeof(float));
    }
    for (int i = 0; i < invarrsize; i++) {
        for (int j = 0; j < invarrsize; j++) {
            arrnull[i][j] = 0;
        }
    }

    //if detmat is not zero, begin the inversion process
    if (detmat != 0) {
        //memory allocation failure flag
        int flag = 0;

        //array2 initialization
        array2 = (float **)malloc((invarrsize) * sizeof(float*));

        for (int i = 0; i < invarrsize; i++) {
            array2[i] = (float *)malloc((invarrsize) * sizeof(float));
            if (array2[i] == NULL) {
                printf("ERROR: Memory allocation failure.\n");
                printf("Inversion unsuccessful, try again.\n");
                flag = 1;
                break;
            }
        }

        //return arrnull if array2 failed to initialize
        if (flag == 1) {
            return arrnull;
        }

        //begin population of the array2
        for (int i = 0; i < invarrsize; i++) {
            for (int j = 0; j < invarrsize; j++) {

                //smaller array miniarr
                float **miniarr;
                
                //counter for the scanning loop
                int i0 = 0, j0 = 0;

                //miniarr initialization
                miniarr = (float **)malloc((miniarrsize) * sizeof(float *));
                for (int mini_i = 0; mini_i < miniarrsize; mini_i++) {
                    miniarr[mini_i] = (float *)malloc((miniarrsize) * sizeof(float));
                    if (miniarr[mini_i] == NULL) {
                        printf("ERROR: Memory allocation failure.\n");
                        flag = 1;
                        break;
                    }
                }

                //return arrnull if miniarr failed to initialize
                if (flag == 1) {
                    return arrnull;
                }

                //scanning loop to populate miniarr
                for (int ii = 0; ii < invarrsize; ii++) {
                    j0 = 0;
                    if (ii != i) {
                        for (int jj = 0; jj < invarrsize; jj++) {
                            if (jj != j) {
                                miniarr[i0][j0] = invarray[ii][jj];
                                j0++;
                            }
                        }
                        i0++;
                    }
                }

                //debugs
                //printf("{ %g, %g, %g, %g }\n", invarray[1][1], invarray[1][2], invarray[2][1], invarray[2][2]);
                //printf("{ %g, %g, %g, %g }\n", miniarr[0][0], miniarr[0][1], miniarr[1][0], miniarr[1][1]);

                //sign incorporation
                if (((i + 1) + (j + 1)) % 2 == 0) {
                    array2[i][j] = det(miniarr, miniarrsize);
                }
                else {
                    array2[i][j] = -det(miniarr, miniarrsize);
                }

                //free the miniarr
                free(miniarr);
            }
        }

        //transpose array2
        transpose(array2, invarrsize);
    }

    //otherwise, return arrnull and tell that the matrix is non-invertible
    else {
        array2 = arrnull;
        printf("\nThe matrix is singular/non-invertible.\n");
    }

    return array2;
}

int main() {
    printf("Available commands are:\ninv\n\n");
    while (true) {

        //Value initialization and reset
        char com[30];
        int size = 0;
        int failed = 0;
        float **arrinv;

        //Available commands are:
        //inv

        //prompt and fetch
        printf("Enter a command: ");
        fgets(com, 500, stdin);

        //chomps
        int len_com = strlen(com);
        if (com[len_com - 1] == 10) {
            com[len_com - 1] = 0;
        }

        //Inverse command
        if (strcmp(com, "inv") == 0) {

            //prompt, fetch, and handle the matrix size
            printf("Enter the dimensions of the square matrix: ");
            int args = scanf("%d", &size);
            if (args != 1) {
                printf("Invalid input.\n");
                scanf("%*[^\n]");
                scanf("%*c");
                continue;
            }
            else if (size < 1) {
                printf("Negative size? Really?\n");
                scanf("%*[^\n]");
                scanf("%*c");
                continue;
            }

            //initialize an array
            float **arr;
            arr = (float **)malloc(size * sizeof(float*));
            for (int i = 0; i < size; i++) {
                int matrixfailure = 0;
                arr[i] = (float *)malloc(size * sizeof(float));
                if (arr[i] == NULL) {
                    printf("ERROR: Memory allocation failure.\n");
                    return EXIT_FAILURE;
                }

                //fetch the user's input to populate the array
                printf("Enter the values for row %d:\n", i+1);
                for (int j = 0; j < size; j++) {
                    int argarr = scanf("%f", &arr[i][j]);
                    if (argarr != 1) {
                        printf("Invalid value.\n");
                        matrixfailure = 1;
                        scanf("%*[^\n]");
                        scanf("%*c");
                        break;
                    }
                }

                //skip the entire process if user inputted a string or char
                if (matrixfailure != 0) {
                    break;
                }
            }

            //otherwise if it hasn't failed to initialize
            if (failed == 0) {
                clock_t time1 = clock();
                //get the determinant
                float detarr = det(arr, size);

                //invert the array
                arrinv = invert(arr, size);
                clock_t time2 = clock();

                //print the inverted array as 1/det(A) * cof(A)
                printf("\nThe inverted matrix is:\n1/%g *\n", detarr);
                for (int i = 0; i < size; i++) {
                    printf("{");
                    for (int j = 0; j < size; j++) {
                        printf(" %.3f ", ((float)((int)(arrinv[i][j]*100000))/100000));
                    }
                    printf("}");

                    if (i != size-1) {
                        printf(",\n");
                    }
                    else {
                        printf("\n");
                    }
                }

                printf("Time taken for the operation: %g\n", (float)(time2 - time1) / CLOCKS_PER_SEC);
            }

            //free the first array and the inverted array
            free(arr);
            free(arrinv);
        }

        //quit program
        else if (strcmp(com, "quit") == 0) {
            printf("Quitting program.\n");
            break;
        }
        
        //invalid command
        else {
            printf("Invalid command.\n");
        }

    //catch failed user input
    scanf("%*[^\n]");
    scanf("%*c");
    }

    return EXIT_SUCCESS;
}