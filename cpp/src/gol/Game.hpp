#include <optional>
#include <vector>

namespace gol {

class Game {
 public:
  static void InitRender();
  static void CleanupRender();

  explicit Game(int width, int height);
  void Seed(std::optional<int> seed = std::nullopt);
  void Step();
  void Render();

 private:
  int width_;
  int height_;
  std::vector<std::vector<bool>> grid_;
};

}  // namespace gol
