namespace TradingLib

open System
open System.Numerics


type Fraction private (n0: bigint, d0: bigint) =
    let n, d =
        assert (d0 <> 0I)
        let (n1, d1) = if (d0 < 0I) then ((-1I * n0), (-1I * d0)) else (n0, d0)
        let g = BigInteger.GreatestCommonDivisor(n1, d1)
        n1/g, d1/g

    member this.N = n
    member this.D = d

    new (ni: int) = Fraction(bigint(ni), 1I)
    new (ni: int, di: int) = Fraction(bigint(ni), bigint(di))
    new (db: float) =
        Fraction(bigint (db * 1000.0 * 1000.0 * 1000.0), 1000I * 1000I * 1000I)

    member this.ToFloat() =
        let db = float((n * 1000I * 1000I * 1000I) / d)
        db / (1000.0 * 1000.0 * 1000.0)

    override this.ToString() = let db = this.ToFloat() in $"%.9f{db}"

    static member ToFloat(f: Fraction) = f.ToFloat()

    static member (+) (n1: Fraction, n2: Fraction) =
        Fraction(n1.N * n2.D + n1.D * n2.N, n1.D * n2.D)

    static member (-) (n1: Fraction, n2: Fraction) =
        Fraction(n1.N * n2.D - n1.D * n2.N, n1.D * n2.D)

    static member (*) (n1: Fraction, n2: Fraction) =
        Fraction(n1.N * n2.N, n1.D * n2.D)

    static member (/) (n1: Fraction, n2: Fraction) =
        Fraction(n1.N * n2.D, n1.D * n2.N)

    static member (==)(n1: Fraction, n2: Fraction) = (n1.N * n2.D = n2.N * n1.D)

    static member Reciprocal (f: Fraction) = Fraction(f.D, f.N)

    interface IAdditionOperators<Fraction, Fraction, Fraction> with
        static member (+) (n1: Fraction, n2: Fraction) = n1 + n2

    interface IMultiplyOperators<Fraction, Fraction, Fraction> with
        static member (*) (n1: Fraction, n2: Fraction) = n1 * n2
