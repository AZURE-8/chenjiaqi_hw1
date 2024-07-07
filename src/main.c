#include <stdio.h>
#include "algebra.h"

int main()
{
    // 主函数，程序的入口点
    while (1)
    {
        char op;
        scanf("%c", &op);
        switch(op) 
        {
            case '+':
            {
                // 处理矩阵加法操作
                int rows, cols;
                scanf("%d %d", &rows, &cols);
                Matrix a = create_matrix(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        scanf("%lf", &a.data[i][j]);
                    }
                }
                scanf("%d %d", &rows, &cols);
                Matrix b = create_matrix(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        scanf("%lf", &b.data[i][j]);
                    }
                }
                Matrix c = add_matrix(a, b);
                print_matrix(c);
                break;
            }
            case '-':
            {
                // 处理矩阵减法操作
                int rows, cols;
                scanf("%d %d", &rows, &cols);
                Matrix a = create_matrix(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        scanf("%lf", &a.data[i][j]);
                    }
                    scanf("\n");
                }
                scanf("%d %d", &rows, &cols);
                Matrix b = create_matrix(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        scanf("%lf", &b.data[i][j]);
                    }
                }
                Matrix c = sub_matrix(a, b);
                print_matrix(c);
                break;
            }
            case '*':
            {
                // 处理矩阵乘法操作
                int rows, cols;
                scanf("%d %d", &rows, &cols);
                Matrix a = create_matrix(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        scanf("%lf", &a.data[i][j]);
                    }
                }
                scanf("%d %d\n", &rows, &cols);
                Matrix b = create_matrix(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        scanf("%lf", &b.data[i][j]);
                    }
                }
                Matrix c = mul_matrix(a, b);
                print_matrix(c);
                break;
            }
            case '.':
            {
                // 处理矩阵缩放操作
                int rows, cols;
                scanf("%d %d", &rows, &cols);
                Matrix a = create_matrix(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        scanf("%lf", &a.data[i][j]);
                    }
                }
                Matrix b = scale_matrix(a, 2.0);
                print_matrix(b);
                break;
            }
            case 't':
            {
                // 处理矩阵转置操作
                int rows, cols;
                scanf("%d %d", &rows, &cols);
                Matrix a = create_matrix(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        scanf("%lf", &a.data[i][j]);
                    }
                }
                Matrix b = transpose_matrix(a);
                print_matrix(b);
                break;
            }
            case 'd':
            {
                // 处理矩阵行列式计算操作
                int rows, cols;
                scanf("%d %d", &rows, &cols);
                Matrix a = create_matrix(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        scanf("%lf", &a.data[i][j]);
                    }
                }
                double det = det_matrix(a);
                printf("%.2f\n", det);
                break;
            }
            case 'i':
            {
                // 处理矩阵求逆操作
                int rows, cols;
                scanf("%d %d", &rows, &cols);
                Matrix a = create_matrix(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        scanf("%lf", &a.data[i][j]);
                    }
                }
                Matrix b = inv_matrix(a);
                print_matrix(b);
                break;
            }
            case 'r':
            {
                // 处理矩阵秩计算操作
                int rows, cols;
                scanf("%d %d", &rows, &cols);
                Matrix a = create_matrix(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        scanf("%lf", &a.data[i][j]);
                    }
                }
                int rank = rank_matrix(a);
                printf("%d\n", rank);
                break;
            }
            case 'j':
            {
                // 处理矩阵迹计算操作
                int rows, cols;
                scanf("%d %d", &rows, &cols);
                Matrix a = create_matrix(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        scanf("%lf", &a.data[i][j]);
                    }
                }
                double trace = trace_matrix(a);
                printf("%.2f\n", trace);
                break;
            }
            case 'q':
            {
                // 退出程序
                return 0;
            }
            default:
            {
                // 忽略无效操作
                continue;
            }
        }
    }
    return 0;
}
