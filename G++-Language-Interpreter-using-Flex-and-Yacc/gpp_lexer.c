#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "y.tab.h"


typedef struct Function
{
    char* name;          // Name of the function
    char** arguments;    // Array of argument names
    char* operation;     // String representation of the operation
    int arg_count;       // Number of arguments

} Function;

// Simple list to store functions
Function** function_list = NULL;
int function_count = 0;

int yylex();
void yyerror();
int gcd(int num1, int num2);
void simplification(int *numerator, int *denominator);
char* add_fractions(char *frac1, char *frac2);
char* sub_fractions(char *frac1, char *frac2);
char* mult_fractions(char *frac1, char *frac2);
char* div_fractions(char *frac1, char *frac2);
void add_function_to_list(Function *function);
void define_function(char* name,char* operation, char** args, int arg_count);
char* execute_function(char *func_name, char **args, int arg_count);
char* create_operation_string(char *op,char *arg1,char *arg2);
char* replace_params_and_evaluate(char* operation, char** params, char** args, int param_count);
char* replace_param(char* operation, const char* param, const char* value);
char* evaluate_replaced_operation(char* operation);

// Error handling function for syntax errors
void yyerror()
{
    fprintf(stderr, "Syntax error!\n");
}

// Calculate greatest common divisor
int gcd(int num1, int num2)
{
    while (num2 != 0)
    {
        int temp = num2;
        num2 = num1 % num2;
        num1 = temp;
    }
    return num1;
}

// Simplify fractions
void simplification(int* numerator, int* denominator)
{
    int gcd_ = gcd(*numerator, *denominator);
    // Perform division and assignment separately
    int simplified_numerator = *numerator / gcd_;
    int simplified_denominator = *denominator / gcd_;

    // Assign the results back
    *numerator = simplified_numerator;
    *denominator = simplified_denominator;
}

// Function to add two fractions
char* add_fractions(char* frac1, char* frac2)
{
    int num1, den1, num2, den2;
    sscanf(frac1, "%db%d", &num1, &den1);
    sscanf(frac2, "%db%d", &num2, &den2);

    if (den1 == 0 || den2 == 0)
    {
        return strdup("Error: denominator cannot be zero\n");
    }

    int common_denom = den1 * den2;
    int new_num1 = num1 * den2;
    int new_num2 = num2 * den1;

    int sum_num = new_num1 + new_num2;

    simplification(&sum_num, &common_denom);

    char *result = (char *)malloc(20); // Allocate a buffer of sufficient size
    sprintf(result, "%db%d", sum_num, common_denom);
    return result;
}

// Function to subtract two fractions
char* sub_fractions(char* frac1, char* frac2)
{
    int num1, den1, num2, den2;
    sscanf(frac1, "%db%d", &num1, &den1);
    sscanf(frac2, "%db%d", &num2, &den2);

    if (den1 == 0 || den2 == 0)
    {
        return strdup("Error: denominator cannot be zero\n");
    }

    int common_denom = den1 * den2;
    int new_num1 = num1 * den2;
    int new_num2 = num2 * den1;

    int diff_num = new_num1 - new_num2;

    simplification(&diff_num, &common_denom);

    char* result = (char* )malloc(20); // Allocate a buffer of sufficient size
    sprintf(result, "%db%d", diff_num, common_denom);
    return result;
}

// Function to multiply two fractions
char* mult_fractions(char* frac1, char* frac2)
{
    int num1, den1, num2, den2;
    sscanf(frac1, "%db%d", &num1, &den1);
    sscanf(frac2, "%db%d", &num2, &den2);

    if (den1 == 0 || den2 == 0)
    {
        return strdup("Error: denominator cannot be zero\n");
    }

    int result_num = num1 * num2;
    int result_den = den1 * den2;

    simplification(&result_num, &result_den);

    char* result = (char* )malloc(20); // Allocate a buffer of sufficient size
    sprintf(result, "%db%d", result_num, result_den);
    return result;
}

// Function to divide two fractions
char* div_fractions(char* frac1, char* frac2)
{
    int num1, den1, num2, den2;
    sscanf(frac1, "%db%d", &num1, &den1);
    sscanf(frac2, "%db%d", &num2, &den2);

    // Error check for denominator 0
    if (den1 == 0 || den2 == 0 || num2 == 0)
    {
        return strdup("Error: denominator cannot be zero and division by zero is not allowed\n");
    }

    int result_num = num1 * den2;
    int result_den = den1 * num2;

    simplification(&result_num, &result_den);

    char* result = (char* )malloc(20); // Allocate a buffer of sufficient size
    sprintf(result, "%db%d", result_num, result_den);
    return result;
}

// Function to add a function to the list of functions
void add_function_to_list(Function* function)
{
    function_list = realloc(function_list, sizeof(Function *) * (function_count + 1));    // Resize the function_list array to accommodate the new function
    function_list[function_count++] = function;    // Add the new function to the list and increment the count
}

// Function to define a mathematical function with a name, operation, arguments, and argument count
void define_function(char* name, char* operation, char** args, int arg_count)
{
    // Allocate memory for a new Function structure
    Function* new_function = malloc(sizeof(Function));
    
    // Memory allocation and copying operations
    new_function->name = malloc(strlen(name) + 1);
    strcpy(new_function->name, name);

    new_function->operation = malloc(strlen(operation) + 1);
    strcpy(new_function->operation, operation);

    new_function->arg_count = arg_count;
    new_function->arguments = malloc(sizeof(char*) * arg_count);

    for (int i = 0; i < arg_count; i++)
    {
        new_function->arguments[i] = malloc(strlen(args[i]) + 1);
        strcpy(new_function->arguments[i], args[i]);
    }

    // Add the new function to the list and print a message
    add_function_to_list(new_function);
    printf("#function %s\n", name);
}

// Function to create a string representing a mathematical operation with two operands
char* create_operation_string(char* op,char* arg1,char* arg2)
{
    int size = snprintf(NULL, 0, "%s %s %s", op, arg1, arg2) + 1;    // Calculate the size required for the resulting string
    char* result = malloc(size);    // Allocate memory for the result string
    sprintf(result, "%s %s %s", op, arg1, arg2);    // Create the operation string
    return result;
}

// Function to execute a mathematical function with given arguments
char* execute_function(char *func_name, char **args, int arg_count)
{
    for (int i = 0; i < function_count; i++)    // Iterate through the list of functions to find the matching one
    {
        if (strcmp(function_list[i]->name, func_name) == 0)        // Check if the function name matches
        {
            // Replace parameters in the operation and evaluate
            char* replaced_operation = replace_params_and_evaluate(function_list[i]->operation, function_list[i]->arguments, args, function_list[i]->arg_count);
            return replaced_operation;
        }
    }
    return strdup("Function not found");
}

// Function to replace parameters in a mathematical operation and evaluate the result
char* replace_params_and_evaluate(char* operation, char** params, char** args, int param_count)
{
    char* replaced_operation = strdup(operation);    // Duplicate the original operation string

    // Replace each parameter with its corresponding argument
    for (int i = 0; i < param_count; i++)
    {
        char* temp = replace_param(replaced_operation, params[i], args[i]);
        free(replaced_operation);
        replaced_operation = temp;
    }

    // Evaluate the replaced operation and return the result
    char* result = evaluate_replaced_operation(replaced_operation);
    free(replaced_operation);
    return result;
}

// Function to replace a specific parameter in a mathematical operation with a given value
char* replace_param(char* operation, const char* param, const char* value)
{
    size_t new_size = strlen(operation) + strlen(value) - strlen(param) + 1;    // Estimate the maximum size of the new string
    char* result = malloc(new_size);    // Allocate memory for the result string
    char* current = result;

    while (*operation)    // Iterate through the original operation string
    {
        if (strncmp(operation, param, strlen(param)) == 0)  // If the parameter is found, replace it with the argument value
        {
            strcpy(current, value);
            current += strlen(value);
            operation += strlen(param);
        }
        else
        {
            *current++ = *operation++;  // Copy the current character from the original string
        }
    }
    *current = '\0';    // Null-terminate the result string
    return result;
}

// Function to evaluate a mathematical operation with replaced parameters
char* evaluate_replaced_operation(char* operation)
{
    // Declare variables to store operation components and result
    char op[100], arg1[100], arg2[100];
    int num1, num2;
    char* result;

    // Parse the created string format
    if (sscanf(operation, "%s %s %s", op, arg1, arg2) == 3)
    {
        // Perform the appropriate mathematical operation based on the operator
        if (strcmp(op, "+") == 0)
        {
            result = add_fractions(arg1, arg2);
        }
        else if (strcmp(op, "-") == 0)
        {
            result = sub_fractions(arg1, arg2);
        }
        else if (strcmp(op, "*") == 0)
        {
            result = mult_fractions(arg1, arg2);
        }
        else if (strcmp(op, "/") == 0)
        {
            result = div_fractions(arg1, arg2);
        }
        else
        {
            return strdup("Unknown operation");
        }
        return result;
    }

    return strdup("Syntax error!");
}



