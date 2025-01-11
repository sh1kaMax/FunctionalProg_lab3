import Interpolation (Message, lagranInterpolation, linearInterpolation)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Interpolation"
    [ testCase "Linear Interpolation 1" $
        linearInterpolation [(0, 0.00), (1.571, 1)] 1 0 @?= [((0.0, 0.0), "\nLinear interpolation:\n"), ((1.0, 0.6365372374283896), ""), ((2.0, 1.2730744748567793), "end")],
      testCase "Linear Interpolation 2" $
        linearInterpolation [(1.571, 1), (3.142, 0)] 1 1.571 @?= [((1.571, 1.0), "\nLinear interpolation:\n"), ((2.5709999999999997, 0.3634627625716105), ""), ((3.5709999999999997, -0.27307447485677905), "end")],
      testCase "Linear Interpolation 3" $
        linearInterpolation [(3.142, 0), (4.712, -1)] 1 3.142 @?= [((3.142, 0.0), "\nLinear interpolation:\n"), ((4.1419999999999995, -0.6369426751592355), ""), ((5.1419999999999995, -1.273885350318471), "end")],
      testCase "Lagrange Interpolation 1" $
        lagranInterpolation [(0, 0.00), (1.571, 1), (3.142, 0)] 1 0 @?= [((0.0, 0.0), "\nLargange Interpolation:\n"), ((1.0, 0.8678948202238131), ""), ((2.0, 0.9254303311816943), ""), ((3.0, 0.1726065328736434), ""), ((4.0, -1.3905765747003394), "end")],
      testCase "Lagrange Interpolation 2" $
        lagranInterpolation [(1.571, 1), (3.142, 0), (4.712, -1)] 1 1.571 @?= [((1.571, 1.0), "\nLargange Interpolation:\n"), ((2.5709999999999997, 0.36353646678820184), ""), ((3.5709999999999997, -0.27318522480044855), ""), ((4.571, -0.9101650747659509), ""), ((5.571, -1.5474030831083057), "end")],
      testCase "Lagrange Interpolation 3" $
        lagranInterpolation [(3.142, 0), (4.712, -1), (12.568, 0)] 1 3.142 @?= [((3.142, 0.0), "\nLargange Interpolation:\n"), ((4.1419999999999995, -0.6831566930870313), ""), ((5.1419999999999995, -1.2041589373046038), ""), ((6.1419999999999995, -1.563006732652717), ""), ((7.1419999999999995, -1.7597000791313708), ""), ((8.142, -1.7942389767405662), ""), ((9.142, -1.6666234254803016), ""), ((10.142, -1.376853425350578), ""), ((11.142, -0.9249289763513954), ""), ((12.142, -0.31085007848275337), ""), ((13.142, 0.46538326825534776), "end")]
    ]
