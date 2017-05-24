defmodule CellularAutomaton.Web.PageController do
  use CellularAutomaton.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
