import java.util.List;

class Knapsack {

    public int maximumValue(int maxCapacity, List<Item> items) {
        int[] maxValuesAtCapacities = new int[maxCapacity + 1];

        for (Item item : items) {
            for (int currentCapacity = maxCapacity; currentCapacity >= item.weight; currentCapacity--) {
                int remainingCapacity = currentCapacity - item.weight;
                int valueWithNewItem = maxValuesAtCapacities[remainingCapacity] + item.value;
                maxValuesAtCapacities[currentCapacity] = Math.max(maxValuesAtCapacities[currentCapacity], valueWithNewItem);
            }
        }

        return maxValuesAtCapacities[maxCapacity];
    }

}