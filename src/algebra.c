#include "algebra.h"
#include <stdio.h>
#include <math.h>

Matrix create_matrix(int row, int col)
{
    Matrix m;
    m.rows = row;
    m.cols = col;
    return m;
}
int max(int a, int b){
    if(a>b){
        return a;
    }
    else{
        return b;
    }
}

Matrix yuzishimartrix(Matrix a, int i, int j)
{
    Matrix new = create_matrix(a.rows - 1, a.cols - 1);
    int m = 0, n = 0;
    for (int row = 0; row < a.rows; row++) {
        if (row == i) continue;
        n = 0;
        for (int col = 0; col < a.cols; col++) {
            if (col == j) continue;
            new.data[m][n] = a.data[row][col];
            n++;
        }
        m++;
    }
    return new;
}

Matrix add_matrix(Matrix a, Matrix b)
{
    // ToDo
    if(a.rows!= b.rows || a.cols!= b.cols)
    {
        printf("Error: Matrix a and b must have the same rows and cols.\n");   
        return create_matrix(0, 0);
    }
    else
    {
        Matrix c = create_matrix(a.rows, a.cols);
        for (int i = 0; i < a.rows; i++)
        {
            for (int j = 0; j < a.cols; j++)
            {
                c.data[i][j] = a.data[i][j] + b.data[i][j];
            }
        
        }
        return c;
    }
}

Matrix sub_matrix(Matrix a, Matrix b)
{
    // ToDo
    if (a.cols!= b.cols || a.rows!= b.rows)
    {
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
    else
    {
        Matrix c = create_matrix(a.rows, a.cols);
        for (int i = 0; i < a.rows; i++)
        {
            for (int j = 0; j < a.cols; j++)
            {
                c.data[i][j] = a.data[i][j] - b.data[i][j];
            }
        
        }
        return c;
    }
}

Matrix mul_matrix(Matrix a, Matrix b)
{
    if(a.cols != b.rows){
    return create_matrix(0, 0);
    printf("Error: The number of cols of matrix a must be equal to the number of rows of matrix b.\n");
    }
    else{
        Matrix c = create_matrix(a.rows, b.cols);
        for (int i = 0; i < a.rows; i++)
        {
            for (int j = 0; j < b.cols; j++)
            {
                for (int k = 0; k < a.cols; k++)
                {
                    c.data[i][j] += a.data[i][k] * b.data[k][j];
                }
            }
        
        }
        return c;
    }    
}


Matrix scale_matrix(Matrix a, double k)
{
    // ToDo
    if( k==0 )
    return create_matrix(0, 0);
    else{
        Matrix c = create_matrix(a.rows, a.cols);
        for (int i = 0; i < a.rows; i++)
        {
            for (int j = 0; j < a.cols; j++)
            {
                c.data[i][j] = a.data[i][j] * k;
            }
        
        }
        return c;
    }
}

Matrix transpose_matrix(Matrix a)
{
    // ToDo
    if(a.rows<=0 || a.cols<=0)
    return create_matrix(0, 0);
    else{
        Matrix c = create_matrix(a.cols, a.rows);
        for (int i = 0; i < a.rows; i++)
        {
            for (int j = 0; j < a.cols; j++)
            {
                c.data[j][i] = a.data[i][j];
            }
        
        }
        return c;
    }
}


double det_matrix(Matrix a)
{
    if (a.rows != a.cols)
     return 0;

    int n = a.rows;
    if (n == 1) return a.data[0][0];
    if (n == 2) return a.data[0][0] * a.data[1][1] - a.data[0][1] * a.data[1][0];

    double d = 0;
    for (int i = 0; i < n; i++) {
        Matrix sub = yuzishimartrix(a, 0, i);
        d += pow(-1, i) * a.data[0][i] * det_matrix(sub);
    }
    return d;
}


Matrix inv_matrix(Matrix a)
{
    double det = det_matrix(a);
    if (a.cols != a.rows || det == 0) {
        printf("Error: The matrix must be a square matrix and its determinant must not be zero.\n");
        return create_matrix(0, 0);
    }

    Matrix c = create_matrix(a.rows, a.cols);
    for (int i = 0; i < a.rows; i++) {
        for (int j = 0; j < a.cols; j++) {
            Matrix sub = yuzishimartrix(a, i, j);
            c.data[j][i] = pow(-1, i + j) * det_matrix(sub) / det; // Note: c.data[j][i] instead of c.data[i][j]。。。全忘了
        }
    }
    return c;
}


int min(int a, int b) {
    return (a < b) ? a : b;
}
void swap_rows(Matrix *a, int row1, int row2) {
    for (int j = 0; j < a->cols; j++) {
        double temp = a->data[row1][j];
        a->data[row1][j] = a->data[row2][j];
        a->data[row2][j] = temp;
    }
}

int rank_matrix(Matrix a) {
    if (a.cols > a.rows) {
        a = transpose_matrix(a);
    }

    int rank = a.cols;
    int m = a.rows;
    int n = a.cols;
    
    for (int col = 0; col < rank; col++) {
        // 找主元
        int count =0;
        for (int row = 0; row < m; row++) {
            
            if (a.data[row][col] != 0) {
                // 交换两行
                count++;
                swap_rows(&a, row, col);
                break;

            }
            else if(count==m-1){
                rank--;        
             }       
    }
    if(count==m-1)
    continue;
     for (int i = 0; i < m - 1; i++) {
            double factor = a.data[i][col] / a.data[m - 1][col];
            for (int j = col; j < n; j++) {
                a.data[i][j] -= factor * a.data[m - 1][j];
            }
        }
    }

    return rank;
}

double trace_matrix(Matrix a)
{
    // ToDo
    if(a.rows!=a.cols){
    printf("Error: The matrix must be a square matrix.\n");
    return 0;
    }
    else{
        double d=0;
        for(int i=0;i<a.rows;i++){
            d+=a.data[i][i];
        }
        return d;
    }
    
}

void print_matrix(Matrix a)
{
    for (int i = 0; i < a.rows; i++)
    {
        for (int j = 0; j < a.cols; j++)
        {
            // 按行打印，每个元素占8个字符的宽度，小数点后保留2位，左对齐
            printf("%-8.2f", a.data[i][j]);
        }
        printf("\n");
    }
}


//写完之后再写一个上三角版本
