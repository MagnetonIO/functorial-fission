module Main where

import FunctorialFission

main :: IO ()
main = do
  putStrLn "Running Functorial Fission tests..."
  
  -- Test conservation laws
  let source = singleton uranium236
      target = configFromList [barium141, krypton92, neutron, neutron, neutron]
      fission = NuclearMorphism source target "test"
  
  if isValidMorphism fission
    then putStrLn "✓ Conservation laws test passed"
    else error "✗ Conservation laws test failed"
  
  -- Test binding energy is positive for stable nuclei
  let be = bindingEnergy standardCoefficients iron56
  if be > 0
    then putStrLn "✓ Binding energy test passed"
    else error "✗ Binding energy test failed"
  
  -- Test energy release is positive for fission
  let energy = energyRelease standardCoefficients fission
  if energy > 0
    then putStrLn "✓ Energy release test passed"
    else error "✗ Energy release test failed"
  
  putStrLn "All tests passed!"
