#include <iostream>
#include <stack>
#include <vector>

using namespace std;

// Partition function to partition the array around a pivot
int partition(vector<int>& arr, int low, int high) {
    int pivot = arr[high];  // Choose the rightmost element as the pivot
    int i = low - 1;  // Index of smaller element

    // Rearrange the array so elements less than the pivot are on the left
    // and elements greater than the pivot are on the right
    for (int j = low; j < high; ++j) {
        if (arr[j] <= pivot) {  // If current element is smaller or equal to pivot
            ++i;
            swap(arr[i], arr[j]);  // Swap elements
        }
    }

    swap(arr[i + 1], arr[high]);  // Swap pivot to its correct position
    return i + 1;  // Return the pivot index
}

// Iterative Quicksort function using a stack to simulate recursion
void quicksort(vector<int>& arr, int low, int high) {
    stack<pair<int, int>> s;  // Stack to store subarray indices
    s.push({low, high});  // Push the initial subarray indices
    
    while (!s.empty()) {
        auto [l, h] = s.top();  // Get the current subarray indices
        s.pop();
        
        if (l < h) {
            int p = partition(arr, l, h);  // Partition the array around the pivot
            s.push({l, p - 1});  // Push left subarray to stack
            s.push({p + 1, h});  // Push right subarray to stack
        }
    }
}

// Function to print the array
void printArray(const vector<int>& arr) {
    for (int num : arr) {
        cout << num << " ";
    }
    cout << endl;
}

int main() {
    // Sample array to be sorted
    vector<int> arr = {9, 7, 5, 11, 12, 2, 14, 3, 10, 6};
    cout << "Original array: ";
    printArray(arr);

    // Call quicksort
    quicksort(arr, 0, arr.size() - 1);

    cout << "Sorted array: ";
    printArray(arr);  // Print the sorted array

    return 0;
}

