# python sudoku.py
DIGITS = set(range(1, 10))

def grid_for(i, j, board):
    r0 = i - i % 3
    c0 =j - j % 3
    return {board[r][c]
        for r in range(r0, r0 + 3)
        for c in range(c0, c0 + 3)}


def candidates(i, j, board):
    used = (set(board[i]) |
        {row[j] for row in board} |
        grid_for(i, j, board))
    used.discard(0)
    return DIGITS - used


def propagate(board):
    changed = True
    while changed:
        changed = False
        for i in range(9):
            for j in range(9):
                if board[i][j] == 0:
                    c = candidates(i, j, board)
                    l = len(c)
                    if l == 0:
                        return False
                    if l == 1:
                        board[i][j] = list(c)[0]
                        changed = True
    return True


def find_mrv_cell(board):
    opts = [((i, j), candidates(i, j, board))
            for i in range(9)
            for j in range(9)
            if board[i][j] == 0]
    return min(opts, key=lambda t: len(t[1]))


def solve(board):
    if not propagate(board):
        return None

    if all(0 not in row for row in board):
        return board

    (i, j), cands = find_mrv_cell(board)
    for v in cands:
        new_board = [row[:] for row in board]
        new_board[i][j] = v
        solved = solve(new_board)
        if solved is not None:
            return solved

    return None


def print_board(board):
    if not board:
        print("Unsolvable")
    print("-" * 25)
    for i, row in enumerate(board):
        if i and i % 3 == 0:
            print("-" * 25)
        print("|", end=" ")
        for j, e in enumerate(row):
            if j and j % 3 == 0:
                print("|", end=" ")
            print(e, end=" ")
        print("|", end="")
        print("")
    print("-" * 25)


print_board(solve([
    [3, 0, 6, 5, 0, 8, 4, 0, 0],
    [5, 2, 0, 0, 0, 0, 0, 0, 0],
    [0, 8, 7, 0, 0, 0, 0, 3, 1],
    [0, 0, 3, 0, 1, 0, 0, 8, 0],
    [9, 0, 0, 8, 6, 3, 0, 0, 5],
    [0, 5, 0, 0, 9, 0, 6, 0, 0],
    [1, 3, 0, 0, 0, 0, 2, 5, 0],
    [0, 0, 0, 0, 0, 0, 0, 7, 4],
    [0, 0, 5, 2, 0, 6, 3, 0, 0]
]))
