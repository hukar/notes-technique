# 06 Udemy Elixir Recursivity

## Recursion

```elixir
defmodule Recursion do
    def countdown(0), do: IO.puts "blast off!"
    def countdown(n) do
        IO.puts n
        countdown(n - 1)
    end
end

Recursion.countdown(5)
```

Exemple plus compliqué

```elixir
defmodule Recursion do
    def countdown(0), do: IO.puts "blast off!"
    def countdown(n) when n == 2 do
        IO.puts "#{n} ...ignite engines..."
        countdown(n - 1)
    end
    def countdown(n) when is_integer n do
        IO.puts n
        countdown(n - 1)
    end
    # is_binary needs brackets
    def countdown(str) when is_binary(str), do: IO.puts "c'mon...countdown is a number!"
end

Recursion.countdown(5)
```

## Tail call optimisation

tail = queue

stack = pile

Dans beaucoup de langage la récursivité pose des problèmes de dépassement de la pile (*stackoverflow*)



### Version non optimisée

```elixir
defmodule Recursion do
    def ten_times(0), do: 0
    def ten_times(n) do
        #not tail optimized
        10 + ten_times(n - 1)
    end
end

IO.puts Recursion.ten_times(3)

# the stack would look like this:
ten_times(3)
10 + ten_times(3 - 1)

(10 + (10 + (10 + ten_times(0)))
```

### Version optimisée

```elixir
#however you may need to accumulate the result
defmodule Recursion do
    def ten_times(n), do: ten_times(n, 0)
    def ten_times(0, acc), do: acc
    def ten_times(n, acc) do
        #tail optimized
        ten_times(n - 1, acc + 10)
    end
end

IO.puts Recursion.ten_times(3)

# stack
ten_times(3, 0)
ten_times(2, 10)
ten_times(1, 20)
ten_times(0, 30)
30
```

