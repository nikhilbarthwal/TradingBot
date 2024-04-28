namespace TradingApp

open TradingLib


type Curve = abstract member Filter: Vector<Bar> * Vector.Buffer<Bar> -> bool

 // Curve for Polynomial & Fourier
