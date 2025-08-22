#ifndef __SPACE_AGE_H__
#define __SPACE_AGE_H__

namespace space_age
{
	static const double EARTH_YEAR = 31557600.0;
	static const double MERCURY_YEAR = EARTH_YEAR * 0.2408467;
	static const double VENUS_YEAR = EARTH_YEAR * 0.61519726;
	static const double MARS_YEAR = EARTH_YEAR * 1.8808158;
	static const double JUPITER_YEAR = EARTH_YEAR * 11.862615;
	static const double SATURN_YEAR = EARTH_YEAR * 29.447498;
	static const double URANUS_YEAR = EARTH_YEAR * 84.016846;
	static const double NEPTUNE_YEAR = EARTH_YEAR * 164.79132;

	class space_age
	{
	public:
		space_age(double seconds) : secs(seconds)
		{
		}
		double seconds() const
		{
			return secs;
		}
		double on_earth() const
		{
			return secs/EARTH_YEAR;
		}
		double on_mercury() const
		{
			return secs/MERCURY_YEAR;
		}
		double on_venus() const
		{
			return secs/VENUS_YEAR;
		}
		double on_mars() const
		{
			return secs/MARS_YEAR;
		}
		double on_jupiter() const
		{
			return secs/JUPITER_YEAR;
		}
		double on_saturn() const
		{
			return secs/SATURN_YEAR;
		}
		double on_uranus() const
		{
			return secs/URANUS_YEAR;
		}
		double on_neptune() const
		{
			return secs/NEPTUNE_YEAR;
		}
	private:
		double secs;
	};
}
#endif