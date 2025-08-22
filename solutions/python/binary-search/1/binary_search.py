def find(search_list, value):
    def binary_search(arr, low, high, value):
        if high >= low: 
            mid = (high + low) // 2 
            if arr[mid] == value:
                return mid
            elif arr[mid] > value:
                return binary_search(arr, low, mid - 1, value)
            else:
                return binary_search(arr, mid + 1, high, value) 
        else:
            raise ValueError("value not in array")
    return binary_search(search_list, 0, len(search_list)-1, value)
