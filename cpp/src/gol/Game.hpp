#include <ncurses.h>
#include <array>
#include <cstdint>
#include <optional>
#include <utility>
#include <vector>

namespace gol {

class Game {
 public:
  static void InitRender();
  static void CleanupRender();

  explicit Game(int width, int height);
  std::uint32_t Seed(std::optional<std::uint32_t> seed = std::nullopt);
  void Step();
  void Render();

 private:
  int width_;
  int height_;
  std::vector<std::vector<bool>> grid_;
  bool nextCellState(int x, int y);
  int countNeighbors(int x, int y);
  bool inBounds(int x, int y);

  inline static constexpr cchar_t CELL_TOP = {.chars = L"\u2580"};
  inline static constexpr cchar_t CELL_BOTTOM = {.chars = L"\u2584"};
  inline static constexpr cchar_t CELL_TOP_BOTTOM = {.chars = L"\u2588"};
  inline static constexpr cchar_t NO_CELL = {.chars = L" "};

  inline static constexpr std::array<std::pair<int, int>, 8> neighborOffsets{
      // clang-format off
      {{-1, -1}, {0, -1}, {1, -1},
       {-1, 0},               {1, 0},
       {-1, 1}, {0, 1}, {1, 1}}
      // clang-format on
  };
};
}  // namespace gol
