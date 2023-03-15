data Func = X -- f(x) = x
          | Sin Func -- f(x) = sen(g(x))
          | Cos Func -- f(x) = cos(g(x))
          | Exp Func -- f(x) = e^g(x)
          | Ln Func  -- f(x) = ln(g(x))
          | Power Func Double -- f(x) = g(x)^n
          | Const Double -- f(x) = k
          | Sum Func Func -- h(x)= f(x) + g(x)
          | Mult Func Func -- h(x) = f(x) * g(x)
          | Div  Func Func -- h(x) = (f(x) / g(x))
          deriving Show

instance Read Func where
  readsPrec _ input = [(Const (read input :: Double), "")]

ddx :: Func -> Double -> Double
ddx X _ = 1
ddx (Sin f) x = cos (eval f x) * ddx f x
ddx (Cos f) x = -sin (eval f x) * ddx f x
ddx (Exp f) x = exp (eval f x) * ddx f x
ddx (Ln f) x = ddx f x / eval f x
ddx (Power f n) x = n * eval f x ** (n - 1) * ddx f x
ddx (Sum f g) x = ddx f x + ddx g x
ddx (Mult (Const k) f) x = k * ddx f x
ddx (Mult f g) x = ddx f x * eval g x + eval f x * ddx g x
ddx (Div f g) x = (ddx f x * eval g x - eval f x * ddx g x) / (eval g x ** 2)
ddx (Const _) _ = 0

eval :: Func -> Double -> Double
eval X x = x
eval (Sin f) x = sin (eval f x)
eval (Cos f) x = cos (eval f x)
eval (Exp f) x = exp (eval f x)
eval (Ln f) x = log (eval f x)
eval (Power f n) x = eval f x ** n
eval (Const k) _ = k
eval (Sum f g) x = eval f x + eval g x
eval (Mult f g) x = eval f x * eval g x
eval (Div f g) x = eval f x / eval g x

main :: IO ()
main = putStrLn "Olá, usuário! Vamos calcular?"

