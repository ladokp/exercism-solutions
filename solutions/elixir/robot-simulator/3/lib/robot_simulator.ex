defmodule RobotSimulator do
  @type robot() :: any()
  @type direction() :: :north | :east | :south | :west
  @type position() :: {integer(), integer()}
  defguardp is_direction(direction) when direction in [:north, :east, :south, :west]
  defguardp is_position(x, y) when is_integer(x) and is_integer(y)

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction, position) :: robot() | {:error, String.t()}
  def create(), do: %{direction: :north, position: {0, 0}}
  def create(direction, _) when not is_direction(direction), do: {:error, "invalid direction"}
  def create(direction, {x, y}) when is_position(x, y), do: %{direction: direction, position: {x, y}}
  def create(_, _) , do: {:error, "invalid position"}

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot, instructions :: String.t()) :: robot() | {:error, String.t()}
  def simulate(robot, instructions) do
    String.graphemes(instructions)
    |> Enum.reduce(robot, &move/2)
  end

  defp move("L", %{direction: :north} = robot), do: %{robot | direction: :west}
  defp move("L", %{direction: :east} = robot), do: %{robot | direction: :north}
  defp move("L", %{direction: :south} = robot), do: %{robot | direction: :east}
  defp move("L", %{direction: :west} = robot), do: %{robot | direction: :south}

  defp move("R", %{direction: :north} = robot), do: %{robot | direction: :east}
  defp move("R", %{direction: :east} = robot), do: %{robot | direction: :south}
  defp move("R", %{direction: :south} = robot), do: %{robot | direction: :west}
  defp move("R", %{direction: :west} = robot), do: %{robot | direction: :north}

  defp move("A", %{direction: :north, position: {x, y}} = robot), do: %{robot | position: {x, y + 1}}
  defp move("A", %{direction: :east, position: {x, y}} = robot), do: %{robot | position: {x + 1, y}}
  defp move("A", %{direction: :south, position: {x, y}} = robot), do: %{robot | position: {x, y - 1}}
  defp move("A", %{direction: :west, position: {x, y}} = robot), do: %{robot | position: {x - 1, y}}
  defp move(_, _), do: {:error, "invalid instruction" }

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot) :: direction()
  def direction(robot) do
    robot.direction
  end

  @doc """
  Return the robot's position.
  """
  @spec position(robot) :: position()
  def position(robot) do
    robot.position
  end
end
