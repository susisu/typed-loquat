import "jest";

import { parseLambda } from "../lambda";

describe("lambda", () => {
    test("variable", () => {
        expect(parseLambda("x").toString()).toBe("x");
    });
    test("application", () => {
        expect(parseLambda("f x").toString()).toBe("(f x)");
        expect(parseLambda("f x y").toString()).toBe("((f x) y)");
    });
    test("abstraction", () => {
        expect(parseLambda("fun x -> f x").toString()).toBe("(fun x -> (f x))");
    });
    test("complex expression", () => {
        const expr = "fun f -> (fun x -> f (x x)) (fun x -> f (x x))";
        expect(parseLambda(expr).toString())
            .toBe("(fun f -> ((fun x -> (f (x x))) (fun x -> (f (x x)))))");
    });
});
