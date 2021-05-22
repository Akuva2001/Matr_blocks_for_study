#include <iostream>
#include <atomic>
#include <thread>
#include <vector>
#include <cstdio>
#include <chrono>
#include <mutex>
#include <unistd.h>
#include <string>
#include <assert.h>
#include <sstream>
#include <algorithm>

typedef int T;

#define BLOCK_SIZE 16

class Table{
public:
    T ** arr;
    int n, m;
    bool allocated = false;
    bool transposed = false;
    Table(){};
    Table(int real_n, int real_m, bool transposed = false):n(real_n), m(real_m), transposed(transposed) {
        //printf("Table constructor %d %d\n", n, m);
        if (transposed){
            std::swap(n,m);
        }
        arr = new T* [n] {};
        for (int i=0; i<n; ++i){
            arr[i] = new T[m] {0};
            //printf("%d ",arr[i][0]);
        }
        //printf("\n");
        allocated = true;
    }
    ~Table(){
        if (!allocated) return;
        for (int i=0; i<n; ++i){
            delete[] arr[i];
        }
        delete[] arr;
    }
    void print(){
        if (!allocated) printf("not allocated\n");
        if (transposed) printf("transposed (you couldn't see it)\n");
        for (int i = 0; i<n; ++i) {
            for (int j = 0; j < m; ++j)
                printf("%3d ",arr[i][j]);
            printf("\n");
        }
    }
    T*&operator[](int i){
        return arr[i];
    }
    friend void mut_and_add(Table& A, Table&B, Table& C){
        assert(C.n == A.n && C.m == B.m && A.m == B.n);
        for (int i=0; i<C.n; ++i){
            for(int j=0; j<C.m; ++j){
                for(int k=0;k<A.m; ++k){
                    C[i][j]+=A[i][k]*B[k][j];
                }
            }
        }
    }

    friend void mut_and_add_right_transposed(Table& A, Table&B, Table& C){
        assert(C.n == A.n && C.m == B.n && A.m == B.m && B.transposed);
        for (int i=0; i<C.n; ++i){
            for(int j=0; j<C.m; ++j){
                for(int k=0;k<A.m; ++k){
                    C[i][j]+=A[i][k]*B[j][k];
                }
            }
        }
    }
};

class Matrix;
void fun(std::atomic<unsigned long long> *it, Matrix *A, Matrix *B, Matrix *C);


class Matrix{
public:
    Table*** arr;
    int block_size = BLOCK_SIZE;
    int n, m;
    int real_size_n, real_size_m;
    bool transposed = false;
    Matrix(int real_n, int real_m, int block_size = BLOCK_SIZE, bool transposed = false):real_size_n(real_n), real_size_m(real_m), block_size(block_size), transposed(transposed) {
        n = real_size_n/block_size + ((real_size_n%block_size>0)?1:0);
        m = real_size_m/block_size + ((real_size_m%block_size>0)?1:0);
        //printf("%d %d\n",n,m);
        arr = new Table** [n] {};
        for (int i=0; i<n; ++i){
            arr[i] = new Table*[m];
            for (int j=0; j<m; ++j){
                arr[i][j]= new Table(((i==n-1 && (real_size_n%block_size>0))?real_size_n%block_size:block_size),
                                 ((j==m-1 && (real_size_m%block_size>0))?real_size_m%block_size:block_size),
                                 transposed);
                //printf("M constructor %d \n", arr[i][j]->arr[0][0]);

            }
        }
    }
    ~Matrix(){
        for (int i=0; i<n; ++i){
            delete[] arr[i];
        }
        delete[] arr;
    }
    Table**& operator[](int i){
        return arr[i];
    }

    friend void mut(Matrix&A, Matrix& B, Matrix&C, int thread_count = 16){
        assert(C.n == A.n && C.m == B.m && A.m == B.n && A.block_size == B.block_size && B.block_size == C.block_size);
        std::atomic<unsigned long long> it {0};
        std::vector<std::thread> th_v(thread_count);
        for (auto& i : th_v)
            i = std::thread(fun, &it, &A, &B, &C);
        for (auto &i : th_v)
            i.join();
    }
    T& operator()(int i, int j){
        assert(i<real_size_n && j<real_size_m);
        return transposed?arr[i/block_size][j/block_size]->arr[j%block_size][i%block_size]:arr[i/block_size][j/block_size]->arr[i%block_size][j%block_size];
    }
    void print(){
        for (int i=0; i<15; ++i) {
            for (int j = 0; j < 15; ++j){
                printf("%3d ", (*this)(i, j));
            }
            printf("\n");
        }
    }
    void blocks_print(){
        for(int i=0; i<n; i++)
            for (int j=0; j<m; ++j){
                printf("%d %d\n", i, j);
                arr[i][j]->print();
            }
    }

};
void fun(std::atomic<unsigned long long> *it, Matrix *A, Matrix *B, Matrix *C){

    for(unsigned long long number = (*it).fetch_add(1, std::memory_order_relaxed);
    number < C->n*C->m;
    number = (*it).fetch_add(1, std::memory_order_relaxed)) {

        int i = number/C->m, j = number%C->m;
        if (B->transposed)
            for(int k = 0; k<A->m; ++k)
                mut_and_add_right_transposed(*(*A)[i][k], *(*B)[k][j], *(*C)[i][j]);
        else
            for(int k = 0; k<A->m; ++k)
                mut_and_add(*(*A)[i][k], *(*B)[k][j], *(*C)[i][j]);

    }
}

class Timer {
public:
    Timer() {
        begin = std::chrono::steady_clock::now();
    }

    ~Timer() {
        auto end = std::chrono::steady_clock::now();
        auto elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(end - begin);
        std::cout << elapsed_ms.count() << " ms" << std::endl;
    }

private:
    std::chrono::time_point<std::chrono::_V2::steady_clock, std::chrono::duration<long int, std::ratio<1, 1000000000>>>
    begin;

};

unsigned long test_func(int N, int M, int block_size, int thread_cnt, bool right_transposed = false){
    Matrix A(N, M, block_size), B(M, N, block_size, right_transposed);//, C(15, 15);

    for (int i=0; i<N; ++i) {
        for (int j = 0; j < M; ++j){
            A(i, j) = B(j, i) = 1;
        }
    }
    Matrix C(N, N, block_size);

    auto begin = std::chrono::steady_clock::now();
    mut(A, B, C, thread_cnt);
    auto end = std::chrono::steady_clock::now();
    auto elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(end - begin);
    printf("thread_count %d, block_size %d, time %lu ms\n", thread_cnt, block_size, elapsed_ms);
    return (*((unsigned long*)&elapsed_ms));

};

int main() {
    std::cout << "Hello, World!" << std::endl;
    for(int i=2; i<1000; i*=2) {
        int block_size = i;
        int N = 1000, M = 1000;
        unsigned long th1 = test_func(N, M, block_size, 1);
        unsigned long th4 = test_func(N, M, block_size, 4);
        std::cout << "faster: " << (double) th1 / th4 << " times\n";
        /*unsigned long th1_t = test_func(N, M, block_size, 1, true);
        unsigned long th4_t = test_func(N, M, block_size, 4, true);
        std::cout << "faster: " << (double) th1_t / th4_t << " times\n";
        std::cout << "transposing faster 1: " << (double) th1 / th1_t << " times\n";
        std::cout << "transposing faster 4: " << (double) th4 / th4_t << " times\n";*/
    }
    return 0;
}