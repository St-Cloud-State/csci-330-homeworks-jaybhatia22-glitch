#include <iostream>
#include <stack>
#include <vector>

using namespace std;

// Partition function to partition the array around a pivot
int partition(vector<int>& arr, int low, int high) {
    int pivot = arr[high];
    int i = low - 1;
    for (int j = low; j < high; ++j) {
        if (arr[j] <= pivot) {
            ++i;
            swap(arr[i], arr[j]);
        }
    }
    swap(arr[i + 1], arr[high]);
    return i + 1;
}

// Iterative Quicksort function using a stack
void quicksort(vector<int>& arr, int low, int high) {
    stack<pair<int, int>> s;
    s.push({low, high});
    
    while (!s.empty()) {
        auto [l, h] = s.top();
        s.pop();
        
        if (l < h) {
            int p = partition(arr, l, h);
            s.push({l, p - 1});
            s.push({p + 1, h});
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
    vector<int> arr = {9, 7, 5, 11, 12, 2, 14, 3, 10, 6};
    cout << "Original array: ";
    printArray(arr);

    quicksort(arr, 0, arr.size() - 1);

    cout << "Sorted array: ";
    printArray(arr);

    return 0;
}
