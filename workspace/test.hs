import ImGui
import ImGui.Enum (c_test, c_test2) --  (c_none, c_notitlebar)

main :: IO ()
main = do
  x <- newImGuiTextBuffer
  print c_test
  print c_test2
--   print c_notitlebar
  pure ()
