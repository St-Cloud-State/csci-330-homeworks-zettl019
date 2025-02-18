// C++ program to implement the iterative quicksort
#include <bits/stdc++.h>
using namespace std;

// Function to swap two elements
void swap(int* a, int* b)
{
    int t = *a;
    *a = *b;
    *b = t;
}

// partition process
int partition(int arr[], int first, int last)
{
    int pivot = arr[last];

    int i = first;
    for (int j = first; j < last; j++) {
        if (arr[j] <= pivot) {
            swap(&arr[i], &arr[j]);
            i++;
        }
    }
    swap(&arr[i], &arr[last]);
    return (i);
}

// iterative quick sort
void quickSortIterative(int arr[], int first, int last)
{
    int stack[last - first + 1];
    int top = -1;
    stack[++top] = first;
    stack[++top] = last;
    while (top >= 0) {
        last = stack[top--];
        first = stack[top--];
        int pivot_pos = partition(arr, first, last);

        // If there are elements on left side of pivot, then
        // push left side to stack
        if (pivot_pos - 1 > first) {
            stack[++top] = first;
            stack[++top] = pivot_pos - 1;
        }

        // If there are elements on right side of pivot,
        // then push right side to stack
        if (pivot_pos + 1 < last) {
            stack[++top] = pivot_pos + 1;
            stack[++top] = last;
        }
    }
}

// driver code
int main()
{
    int size = 10;
    int arr[size] = { 10, 23, 4, 0, 8, 5, 9, 11, 22, 3 };

    quickSortIterative(arr, 0, size - 1);

    cout << "Elements after sorting: ";
    for (int i = 0; i < size; i++) {
        cout << arr[i] << " ";
    }
    cout << "\n"; 
    return 0;
};