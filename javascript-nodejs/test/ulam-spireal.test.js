const {main, joinArray} = require('../bin/main')
describe("ulamSpiralTest", () => {

    test("verify generated spiral 6*6", async ()=> {
        return main(6).then(value => {
            let checkElement = value[3][2]
            expect(checkElement.number).toBe(1)

            checkElement = value[0][3]
            expect(checkElement.number).toBe(33)

            checkElement = value[5][3]
            expect(checkElement.number).toBe(24)

            const expected = "-----*\n*---*-\n-*-*-*\n*--**-\n-*----\n--*---";
            const joined = joinArray(value)
            expect(joined).toBe(expected)
        })
    })
})