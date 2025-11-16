#include <ncurses.h>
#include <array>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace gol {

#define COLOR_BG 10
#define COLOR_FG_0 11
#define COLOR_FG_1 12

struct Cell {
  bool isAlive;
  int age = 0;
};

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
  std::vector<std::vector<Cell>> grid_;
  Cell nextCellState(int x, int y);
  int countNeighbors(int x, int y);
  bool inBounds(int x, int y);
  static void initHexColor(short colorId, const std::string& hex);
  static short colorFromHex(const std::string& hex);

  inline static constexpr wchar_t CELL_TOP[] = L"\u2580";
  inline static constexpr wchar_t CELL_BOTTOM[] = L"\u2584";
  inline static constexpr wchar_t CELL_TOP_BOTTOM[] = L"\u2588";
  inline static constexpr wchar_t NO_CELL[] = L" ";

  inline static constexpr std::array<std::pair<int, int>, 8> neighborOffsets{
      // clang-format off
      {{-1, -1}, {0, -1}, {1, -1},
       {-1, 0},               {1, 0},
       {-1, 1}, {0, 1}, {1, 1}}
      // clang-format on
  };
};
}  // namespace gol
