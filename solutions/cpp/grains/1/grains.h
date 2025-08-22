#if !defined(GRAINS_H)
#define GRAINS_H

namespace grains {
    constexpr unsigned long long square(unsigned n) {
        return (n < 2) ? 1 : 2 * square(n - 1);
    }

    constexpr unsigned long long total(unsigned n = 64) {
        return (n < 2) ? 1 : square(n) + total(n - 1);
    }
}  // namespace grains

#endif // GRAINS_H