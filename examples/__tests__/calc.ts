import "jest";

import { calcExpr } from "../calc";

describe("calc", () => {
    test("addition", () => {
        expect(calcExpr("3 + 2")).toBe(5);
    });
    test("subtraction", () => {
        expect(calcExpr("3 - 2")).toBe(1);
    });
    test("mulitplication", () => {
        expect(calcExpr("3 * 2")).toBe(6);
    });
    test("division", () => {
        expect(calcExpr("3 / 2")).toBe(1.5);
    });
    test("exponentiation", () => {
        expect(calcExpr("3 ** 2")).toBe(9);
    });
    test("unary plus", () => {
        expect(calcExpr("+1")).toBe(1);
    });
    test("unary minus", () => {
        expect(calcExpr("-1")).toBe(-1);
    });
    test("operator precedence", () => {
        expect(calcExpr("1 + 2 * 3")).toBe(7);
    });
    test("parentheses", () => {
        expect(calcExpr("(1 + 2) * 3")).toBe(9);
    });
});
