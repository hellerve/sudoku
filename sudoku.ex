# elixir sudoku.exs
defmodule Sudoku do
  @digits MapSet.new(1..9)

  def solve(board) do
    case propagate(board) do
      {:error, :contradiction} -> {:error, :unsat}
      {:ok, b} ->
        if solved?(b) do
          {:ok, b}
        else
          {{i, j}, cs} = find_mrv(b)

          branch(b, i, j, cs)
        end
    end
  end

  defp propagate(board) do
    {b2, changed, ok?} =
      Enum.reduce(0..8, {board, false, true}, fn i, acc ->
        Enum.reduce(0..8, acc, fn j, {b, ch, ok} ->
          v = b |> Enum.at(i) |> Enum.at(j)

          cond do
            not ok -> {b, ch, false}
            v != 0 -> {b, ch, ok}
            true ->
              cs = candidates(b, i, j)
              cond do
                cs == [] -> {b, ch, false}
                length(cs) == 1 ->
                  {put_cell(b, i, j, hd(cs)), true, ok}
                true -> {b, ch, ok}
              end
          end
        end)
      end)

    cond do
      not ok? -> {:error, :contradiction}
      changed -> propagate(b2)
      true    -> {:ok, b2}
    end
  end

  defp branch(b, i, j, cs) do

    cs
    |> Task.async_stream(
         fn v -> solve(put_cell(b, i, j, v)) end,
         max_concurrency: System.schedulers_online(),
         timeout: :infinity,
         ordered: false
       )
    |> Enum.find_value(fn
         {:ok, {:ok, s}} -> {:ok, s}
         _ -> nil
       end)
    || {:error, :unsat}
  end

  defp solved?(b), do: Enum.all?(b, fn row -> Enum.all?(row, &(&1 != 0)) end)

  defp candidates(b, i, j) do
    used =
      MapSet.new(row(b, i) ++ col(b, j) ++ box(b, i, j))
      |> MapSet.delete(0)

    MapSet.to_list(MapSet.difference(@digits, used))
  end

  defp row(b, i), do: Enum.at(b, i)
  defp col(b, j), do: Enum.map(b, &Enum.at(&1, j))

  defp box(b, i, j) do
    r0 = div(i, 3) * 3
    c0 = div(j, 3) * 3
    for r <- r0..(r0 + 2), c <- c0..(c0 + 2), do: b |> Enum.at(r) |> Enum.at(c)
  end

  defp find_mrv(b) do
    opts =
      for i <- 0..8, j <- 0..8, Enum.at(Enum.at(b, i), j) == 0 do
        cs = candidates(b, i, j)
        {{i, j}, cs}
      end

    Enum.min_by(opts, fn {_pos, cs} -> length(cs) end)
  end

  defp put_cell(b, i, j, v), do: put_in(b, [Access.at(i), Access.at(j)], v)

  def pretty(b) do
    Enum.with_index(b)
    |> Enum.each(fn {row, _} ->
      row
      |> Enum.with_index()
      |> Enum.map(fn {x, _} ->
        [Integer.to_string(x), " "]
      end)
      |> IO.iodata_to_binary()
      |> IO.puts()
    end)
  end
end

puzzle = [
  [3,0,6,5,0,8,4,0,0],
  [5,2,0,0,0,0,0,0,0],
  [0,8,7,0,0,0,0,3,1],
  [0,0,3,0,1,0,0,8,0],
  [9,0,0,8,6,3,0,0,5],
  [0,5,0,0,9,0,6,0,0],
  [1,3,0,0,0,0,2,5,0],
  [0,0,0,0,0,0,0,7,4],
  [0,0,5,2,0,6,3,0,0]
]

case Sudoku.solve(puzzle) do
  {:ok, b} -> Sudoku.pretty(b)
  _ -> IO.puts("unsatisfiable")
end
