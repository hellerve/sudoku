'pharo Pharo.image st sudoku.st
The gt*For: methods are moldable Glamorous Toolkit views and only render inside Glamorous Toolkit.'!

Object subclass: #SudokuCell
	instanceVariableNames: 'value position groups'
	classVariableNames: ''
	package: 'Sudoku'!

Object subclass: #SudokuGroup
	instanceVariableNames: 'cells'
	classVariableNames: ''
	package: 'Sudoku'!

Object subclass: #SudokuBoard
	instanceVariableNames: 'cells'
	classVariableNames: ''
	package: 'Sudoku'!

!SudokuCell methodsFor: 'groups'!
addGroup: aGroup
	groups add: aGroup


! !

!SudokuCell methodsFor: 'solving'!
candidates
	| used |
	used := groups inject: Set new into: [:acc :g | acc union: g placedValues].
	^(1 to: 9) asSet difference: used


! !

!SudokuCell methodsFor: 'gt views'!
gtCandidatesFor: aView
	<gtView>
	^aView list
		title: 'Candidates';
		items: [ self candidates asSortedCollection ];
		yourself


! !

!SudokuCell methodsFor: 'initialization'!
initialize
	value := 0.
	groups := OrderedCollection new


! !

!SudokuCell methodsFor: 'testing'!
isEmpty
	^value = 0


! !

!SudokuCell methodsFor: 'testing'!
isPlaced
	^value > 0


! !

!SudokuCell methodsFor: 'accessing'!
position
	^position


! !

!SudokuCell methodsFor: 'accessing'!
position: aPoint
	position := aPoint


! !

!SudokuCell methodsFor: 'accessing'!
value
	^value


! !

!SudokuCell methodsFor: 'accessing'!
value: anInteger
	value := anInteger


! !

!SudokuGroup methodsFor: 'accessing'!
addCell: aCell
	cells add: aCell


! !

!SudokuGroup methodsFor: 'gt views'!
gtCellsFor: aView
	<gtView>
	^aView columnedList
		title: 'Cells';
		items: [ cells ];
		column: 'Position' text: [:c | c position printString] width: 80;
		column: 'Value' text: [:c | c isEmpty ifTrue: ['—'] ifFalse: [c value printString]] width: 60;
		yourself


! !

!SudokuGroup methodsFor: 'initialization'!
initialize
	cells := OrderedCollection new


! !

!SudokuGroup methodsFor: 'accessing'!
placedValues
	^(cells select: [:c | c isPlaced]) collect: [:c | c value] as: Set


! !

!SudokuBoard methodsFor: 'accessing'!
at: aPoint
	^cells at: (aPoint x - 1) * 9 + aPoint y


! !

!SudokuBoard methodsFor: 'accessing'!
cellAt: anIndex
	^cells at: anIndex


! !

!SudokuBoard methodsFor: 'copying'!
copy
	| newBoard |
	newBoard := self class new.
	1 to: 81 do: [:k |
		(newBoard cellAt: k) value: (self cellAt: k) value].
	^newBoard


! !

!SudokuBoard methodsFor: 'solving'!
emptyCells
	^cells select: [:c | c isEmpty]


! !

!SudokuBoard methodsFor: 'gt views'!
gtGridFor: aView
	<gtView>
	^aView mondrian
		title: 'Grid';
		painting: [:m |
			m nodes
				shape: [:cell |
					| el boxRow boxCol bg |
					boxRow := (cell position x - 1) // 3.
					boxCol := (cell position y - 1) // 3.
					bg := (boxRow + boxCol) \\ 2 = 0
						ifTrue: [Color white]
						ifFalse: [Color veryLightGray].
					el := BlElement new
						size: 42 @ 42;
						border: (BlBorder paint: Color gray width: 1);
						background: bg;
						layout: BlFrameLayout new.
					cell isPlaced ifTrue: [
						el addChild: (BlTextElement new
							text: cell value printString asRopedText;
							constraintsDo: [:c |
								c frame horizontal alignCenter.
								c frame vertical alignCenter])].
					el];
				with: cells.
			m layout grid ];
		yourself


! !

!SudokuBoard methodsFor: 'initialization'!
initialize
	| rows cols boxes |
	cells := (1 to: 81) collect: [:_ | SudokuCell new].
	rows  := (1 to: 9) collect: [:_ | SudokuGroup new].
	cols  := (1 to: 9) collect: [:_ | SudokuGroup new].
	boxes := (1 to: 9) collect: [:_ | SudokuGroup new].
	1 to: 9 do: [:i |
		1 to: 9 do: [:j |
			| cell bi |
			cell := self at: i@j.
			cell position: i@j.
			bi := ((i - 1) // 3) * 3 + ((j - 1) // 3) + 1.
			cell addGroup: (rows  at: i).
			cell addGroup: (cols  at: j).
			cell addGroup: (boxes at: bi).
			(rows  at: i) addCell: cell.
			(cols  at: j) addCell: cell.
			(boxes at: bi) addCell: cell]]


! !

!SudokuBoard methodsFor: 'solving'!
isSolved
	^cells allSatisfy: [:c | c isPlaced]


! !

!SudokuBoard methodsFor: 'solving'!
mrvCell
	^self emptyCells
		ifEmpty: [nil]
		ifNotEmpty: [:empties |
			empties inject: empties first into: [:best :cell |
				cell candidates size < best candidates size
					ifTrue:  [cell]
					ifFalse: [best]]]


! !

!SudokuBoard methodsFor: 'printing'!
printOn: aStream
	| sep |
	sep := '-------------------------'.
	aStream nextPutAll: sep; nextPut: Character lf.
	1 to: 9 do: [:i |
		(i > 1 and: [(i - 1) \\ 3 = 0]) ifTrue: [
			aStream nextPutAll: sep; nextPut: Character lf].
		aStream nextPutAll: '| '.
		1 to: 9 do: [:j |
			(j > 1 and: [(j - 1) \\ 3 = 0]) ifTrue: [aStream nextPutAll: '| '].
			aStream nextPutAll: (self at: i@j) value printString , ' '].
		aStream nextPutAll: '|'; nextPut: Character lf].
	aStream nextPutAll: sep; nextPut: Character lf


! !

!SudokuBoard methodsFor: 'solving'!
propagate
	| changed cands |
	changed := true.
	[changed] whileTrue: [
		changed := false.
		self emptyCells do: [:cell |
			cands := cell candidates.
			cands isEmpty ifTrue: [^false].
			cands size = 1 ifTrue: [
				cell value: cands anyOne.
				changed := true]]].
	^true


! !

!SudokuBoard methodsFor: 'solving'!
solve
	| board cell result |
	board := self copy.
	board propagate ifFalse: [^nil].
	board isSolved ifTrue: [^board].
	cell := board mrvCell.
	cell candidates do: [:v |
		| newBoard |
		newBoard := board copy.
		(newBoard at: cell position) value: v.
		result := newBoard solve.
		result isNil ifFalse: [^result]].
	^nil


! !

!SudokuBoard class methodsFor: 'instance creation'!
from: aNestedArray
	| board |
	board := self new.
	1 to: 9 do: [:i |
		1 to: 9 do: [:j |
			(board at: i@j) value: ((aNestedArray at: i) at: j)]].
	^board


! !

Transcript show: (SudokuBoard from: #(
	#(0 0 3 0 2 0 6 0 0)
	#(9 0 0 3 0 5 0 0 1)
	#(0 0 1 8 0 6 4 0 0)
	#(0 0 8 1 0 2 9 0 0)
	#(7 0 0 0 0 0 0 0 8)
	#(0 0 6 7 0 8 2 0 0)
	#(0 0 2 6 0 9 5 0 0)
	#(8 0 0 2 0 3 0 0 9)
	#(0 0 5 0 1 0 3 0 0)
)) solve printString; cr!
