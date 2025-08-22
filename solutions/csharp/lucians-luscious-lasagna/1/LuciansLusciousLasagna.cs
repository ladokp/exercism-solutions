class Lasagna
{
    public int ExpectedMinutesInOven()
    {
        return 40;
    }

    public int RemainingMinutesInOven(int actualMinutesInOven)
    {
        return this.ExpectedMinutesInOven() - actualMinutesInOven;
    }    

    public int PreparationTimeInMinutes(int layers)
    {
        return 2 * layers;
    }        

    public int ElapsedTimeInMinutes(int layers, int actualMinutesInOven)
    {
        return this.PreparationTimeInMinutes(layers) + actualMinutesInOven;
    }
}
