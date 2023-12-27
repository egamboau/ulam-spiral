import unittest
import ulam_spiral


class TestUlamSpiral(unittest.TestCase):
    def test_spiral_6_by_6(self):
        spiral_to_test = ulam_spiral.main(6)
        checkElement = spiral_to_test[3][2]
        self.assertEqual(checkElement.get("number"), 1)
        
        checkElement = spiral_to_test[0][3]
        self.assertEqual(checkElement.get("number"), 33)

        checkElement = spiral_to_test[5][3]
        self.assertEqual(checkElement.get("number"), 24)

        expected = "-----*\n*---*-\n-*-*-*\n*--**-\n-*----\n--*---"
        join_result = ulam_spiral.join_array(spiral_to_test)
        self.assertEqual(expected, join_result)

if __name__ == '__main__':
    unittest.main()

