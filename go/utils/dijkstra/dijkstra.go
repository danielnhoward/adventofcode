package dijkstra

import "sort"

func GetNextJob(queue [][]int) ([]int, [][]int) {
	sort.Slice(queue, func(i int, j int) bool {
		return queue[i][0] < queue[j][0]
	})

	return queue[0], queue[1:]
}
