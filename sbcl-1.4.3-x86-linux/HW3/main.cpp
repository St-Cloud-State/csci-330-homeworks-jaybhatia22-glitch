#include <iostream>
#include <stack>
using namespace std;

// This function partitions the array around a pivot and returns the pivot index
int partitionArray(int arr[], int start, int end) {
    int pivot = arr[end]; // Choose the pivot as the last element
    int i = start - 1;    // Index for the smaller element
    
    for (int j = start; j < end; ++j) {  // Traverse the array
        if (arr[j] <= pivot) {  // If current element is less than or equal to pivot
            ++i;
            swap(arr[i], arr[j]);  // Swap elements
        }
    }
    
    swap(arr[i + 1], arr[end]);  // Move pivot to its correct position
    return i + 1;
}

// This function performs the iterative version of Quicksort using two stacks
void iterativeQuickSort(int arr[], int start, int end) {
    stack<int> startStack, endStack;
    
    startStack.push(start);
    endStack.push(end);
    
    while (!startStack.empty()) {  // Continue until all subarrays are sorted
        int low = startStack.top();
        int high = endStack.top();
        startStack.pop();
        endStack.pop();
        
        if (low < high) {
            int pivotIndex = partitionArray(arr, low, high);  // Find the pivot index
            
            // Push the right subarray to the stack
            if (pivotIndex + 1 < high) {
                startStack.push(pivotIndex + 1);
                endStack.push(high);
            }
            
            // Push the left subarray to the stack
            if (low < pivotIndex - 1) {
                startStack.push(low);
                endStack.push(pivotIndex - 1);
            }
        }
    }
}

int main() {
    int numbers[] = {33, 10, 55, 71, 18, 92, 46};
    int n = sizeof(numbers) / sizeof(numbers[0]);
    
    // Perform quicksort
    iterativeQuickSort(numbers, 0, n - 1);
    
    // Display the sorted array
    cout << "Sorted array: ";
    for (int i = 0; i < n; ++i) {
        cout << numbers[i] << " ";
    }
    cout << endl;

    return 0;
}
