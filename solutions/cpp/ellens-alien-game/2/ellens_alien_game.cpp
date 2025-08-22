namespace targets {
    class Alien {
      public:
        Alien(int x, int y) {
          teleport(x, y);
        }
    
        int get_health() {
          return health;
        }
    
        bool hit() {
            if (is_alive()) {
                health--;
            }
            return true;
        }
    
        bool is_alive() {
            return health > 0;
        }
    
        bool teleport(int x, int y) {
            x_coordinate = x;
            y_coordinate = y;
            return true;
        }
    
        bool collision_detection(Alien opponent) {
            return x_coordinate == opponent.x_coordinate && y_coordinate == opponent.y_coordinate;
        }
        int x_coordinate;
        int y_coordinate;
      private:        
        int health{3};
    };
}  // namespace targets