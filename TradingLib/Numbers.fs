namespace TradingLib

open System


type Fraction private (n0: bigint, d0: bigint) =
    let n, d =
        assert (d0 <> 0I)
        let n1, d1 = if (d0 < 0I) then ((-1I * n0), (-1I * d0)) else (n0, d0)
        let g = System.Numerics.BigInteger.GreatestCommonDivisor(n1, d1)
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

    override this.ToString() = let db = this.ToFloat() in $"%.6f{db}"

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


type Complex(real: float, imaginary: float) =
    member this.Real = real
    member this.Imaginary = imaginary

    new (phase: float) = Complex(Math.Cos(phase) , Math.Sin(phase))

    static member (/) (c: Complex, f: float) = Complex(c.Real / f, c.Imaginary / f)
    static member Pi: float = 3.141593

    static member (+) (c1: Complex, c2: Complex) =
        Complex(c1.Real + c2.Real, c1.Imaginary + c2.Imaginary)

    static member (-) (c1: Complex, c2: Complex) =
        Complex(c1.Real - c2.Real, c1.Imaginary - c2.Imaginary)

    static member (*) (c1: Complex, c2: Complex) =
        let real = c1.Real * c2.Real - c1.Imaginary * c2.Imaginary
        let imaginary = c1.Real * c2.Imaginary + c1.Imaginary * c2.Real
        Complex(real, imaginary)

    override this.ToString() = $"{this.Real} + {this.Imaginary}i"
