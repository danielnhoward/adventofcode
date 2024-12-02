package utils

import "math"

type Coordinate struct {
	X int
	Y int
}

var Directions [][]int = [][]int{
	{0, 1},
	{0, -1},
	{1, 0},
	{-1, 0},
}

func AxisGetDistanceBetweenPoints(pointA Coordinate, pointB Coordinate) float64 {
	length := Coordinate{
		X: pointA.X - pointB.X,
		Y: pointA.Y - pointB.Y,
	}

	lengthSquared := length.X*length.X + length.Y*length.Y
	return math.Sqrt(float64(lengthSquared))
}

func AxisCoordinatesMatch(a Coordinate, b Coordinate) bool {
	return a.X == b.X && a.Y == b.Y
}

func AxisConvertCoordinateToSlice(a Coordinate) []int {
	return []int{a.X, a.Y}
}

func AxisConvertSliceToCoordinate(a []int) Coordinate {
	return Coordinate{X: a[0], Y: a[1]}
}
