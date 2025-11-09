#include <cstdint>
#include <optional>
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
};

}  // namespace gol
