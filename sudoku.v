// iverilog -o sudoku sudoku.v && vvp sudoku

module sudoku (
  input  wire         clk,
  input  wire         start,
  input  wire [323:0] puzzle,
  output reg          done,
  output reg          solved,
  output reg  [323:0] board
);
  localparam RUN = 2'd0, BACK = 2'd1, HALT = 2'd2;

  function [6:0] member(input integer u, input integer k);
    if (u < 9)       member = u * 9 + k;
    else if (u < 18) member = k * 9 + (u - 9);
    else             member = ((u - 18) / 3) * 27 + ((u - 18) % 3) * 3
                            + (k / 3) * 9 + k % 3;
  endfunction

  function [3:0] digit(input [8:0] onehot);
    integer d;
    begin
      digit = 0;
      for (d = 1; d <= 9; d = d + 1)
        if (onehot[d - 1]) digit = d;
    end
  endfunction

  function [3:0] popcount(input [8:0] m);
    integer b;
    begin
      popcount = 0;
      for (b = 0; b < 9; b = b + 1)
        popcount = popcount + m[b];
    end
  endfunction

  function [3:0] at(input [323:0] b, input integer i);
    at = b[4 * i +: 4];
  endfunction

  function [8:0] bit_of(input [3:0] d);
    reg [9:0] wide;
    begin
      wide = 10'b1 << d;
      bit_of = wide[9:1];
    end
  endfunction

  reg [242:0] seen;
  reg [26:0]  clash;
  reg [728:0] cands;
  reg [323:0] forced;
  reg         dead, full, any_forced;
  reg [6:0]   best;
  reg [3:0]   best_n;
  integer     u, k, i;

  always @* begin
    for (u = 0; u < 27; u = u + 1) begin
      seen[9 * u +: 9] = 0;
      clash[u] = 0;
      for (k = 0; k < 9; k = k + 1) begin
        clash[u] = clash[u] | |(seen[9 * u +: 9] & bit_of(at(board, member(u, k))));
        seen[9 * u +: 9] = seen[9 * u +: 9] | bit_of(at(board, member(u, k)));
      end
    end

    dead = |clash;
    full = 1;
    any_forced = 0;
    forced = board;
    best = 0;
    best_n = 10;
    for (i = 0; i < 81; i = i + 1) begin
      cands[9 * i +: 9] = 0;
      if (at(board, i) == 0) begin
        full = 0;
        cands[9 * i +: 9] = ~(seen[9 * (i / 9) +: 9]
                            | seen[9 * (9 + i % 9) +: 9]
                            | seen[9 * (18 + (i / 27) * 3 + (i % 9) / 3) +: 9]);
        if (popcount(cands[9 * i +: 9]) == 0) dead = 1;
        if (popcount(cands[9 * i +: 9]) == 1) begin
          any_forced = 1;
          forced[4 * i +: 4] = digit(cands[9 * i +: 9]);
        end
        if (popcount(cands[9 * i +: 9]) < best_n) begin
          best = i;
          best_n = popcount(cands[9 * i +: 9]);
        end
      end
    end
  end

  wire [8:0] pick = cands[9 * best +: 9];

  reg [323:0] saved [0:80];
  reg [6:0]   where [0:80];
  reg [8:0]   rest  [0:80];
  reg [6:0]   sp;
  reg [1:0]   state;

  wire [8:0] left = rest[sp - 1];
  wire [8:0] low = left & -left;

  always @(posedge clk) begin
    if (start) begin
      board  <= puzzle;
      sp     <= 0;
      done   <= 0;
      solved <= 0;
      state  <= RUN;
    end else case (state)
      RUN:
        if (dead) state <= BACK;
        else if (any_forced) board <= forced;
        else if (full) begin
          done   <= 1;
          solved <= 1;
          state  <= HALT;
        end else begin
          saved[sp] <= board;
          where[sp] <= best;
          rest[sp]  <= pick & (pick - 1);
          sp        <= sp + 1;
          board[4 * best +: 4] <= digit(pick & -pick);
        end
      BACK:
        if (sp == 0) begin
          done  <= 1;
          state <= HALT;
        end else if (left == 0) sp <= sp - 1;
        else begin
          board <= saved[sp - 1];
          board[4 * where[sp - 1] +: 4] <= digit(low);
          rest[sp - 1] <= left & (left - 1);
          state <= RUN;
        end
    endcase
  end
endmodule

module main;
  reg          clk = 0;
  reg          start = 1;
  reg  [323:0] puzzle;
  wire         done, solved;
  wire [323:0] board;
  integer      i, cycles = 0;

  reg [3:0] grid [0:80];
  initial begin
    {grid[ 0], grid[ 1], grid[ 2], grid[ 3], grid[ 4], grid[ 5], grid[ 6], grid[ 7], grid[ 8]} = {4'd8, 4'd0, 4'd0, 4'd0, 4'd0, 4'd0, 4'd0, 4'd0, 4'd0};
    {grid[ 9], grid[10], grid[11], grid[12], grid[13], grid[14], grid[15], grid[16], grid[17]} = {4'd0, 4'd0, 4'd3, 4'd6, 4'd0, 4'd0, 4'd0, 4'd0, 4'd0};
    {grid[18], grid[19], grid[20], grid[21], grid[22], grid[23], grid[24], grid[25], grid[26]} = {4'd0, 4'd7, 4'd0, 4'd0, 4'd9, 4'd0, 4'd2, 4'd0, 4'd0};
    {grid[27], grid[28], grid[29], grid[30], grid[31], grid[32], grid[33], grid[34], grid[35]} = {4'd0, 4'd5, 4'd0, 4'd0, 4'd0, 4'd7, 4'd0, 4'd0, 4'd0};
    {grid[36], grid[37], grid[38], grid[39], grid[40], grid[41], grid[42], grid[43], grid[44]} = {4'd0, 4'd0, 4'd0, 4'd0, 4'd4, 4'd5, 4'd7, 4'd0, 4'd0};
    {grid[45], grid[46], grid[47], grid[48], grid[49], grid[50], grid[51], grid[52], grid[53]} = {4'd0, 4'd0, 4'd0, 4'd1, 4'd0, 4'd0, 4'd0, 4'd3, 4'd0};
    {grid[54], grid[55], grid[56], grid[57], grid[58], grid[59], grid[60], grid[61], grid[62]} = {4'd0, 4'd0, 4'd1, 4'd0, 4'd0, 4'd0, 4'd0, 4'd6, 4'd8};
    {grid[63], grid[64], grid[65], grid[66], grid[67], grid[68], grid[69], grid[70], grid[71]} = {4'd0, 4'd0, 4'd8, 4'd5, 4'd0, 4'd0, 4'd0, 4'd1, 4'd0};
    {grid[72], grid[73], grid[74], grid[75], grid[76], grid[77], grid[78], grid[79], grid[80]} = {4'd0, 4'd9, 4'd0, 4'd0, 4'd0, 4'd0, 4'd4, 4'd0, 4'd0};
    for (i = 0; i < 81; i = i + 1) puzzle[4 * i +: 4] = grid[i];
  end

  sudoku s(.clk(clk), .start(start), .puzzle(puzzle),
           .done(done), .solved(solved), .board(board));

  always #5 clk = ~clk;

  initial begin
    @(negedge clk) start = 0;
    while (!done) begin
      @(posedge clk) cycles = cycles + 1;
      #1;
    end
    if (solved)
      for (i = 0; i < 81; i = i + 1)
        $write("%0d%s", board[4 * i +: 4], i % 9 == 8 ? "\n" : " ");
    else
      $display("no solution");
    $display("%0d cycles", cycles);
    $finish;
  end
endmodule
