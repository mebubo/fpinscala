interface RNG {
    nextInt(): [number, RNG];
}

class SimpleRNG implements RNG {
    constructor(public seed: number) {
    }
    nextInt(): [number, RNG] {
        const newSeed = (this.seed * 0x5DEECE66D + 0xB) & 0xFFFFFFFFFFFF;
        const nextRNG = new SimpleRNG(newSeed);
        const n = (newSeed >>> 16);
        return [n, nextRNG]
    }
}

function nonNegativeInt(rng: RNG): [number, RNG] {
    const [i, rng1] = rng.nextInt();
    return [i >= 0 ? i : -(i+1), rng1];
}

const MAX_SAFE_INTEGER = 9007199254740991;

function double(rng: RNG): [number, RNG] {
    const [i, rng1] = rng.nextInt();
    return [i/(MAX_SAFE_INTEGER + 1), rng1]
}



type Rand<A> = (rng: RNG) => [A, RNG];

function unit<A>(a: A): Rand<A> {
    return (rng: RNG) => [a, rng];
}

const int: Rand<number> = (rng: RNG) => rng.nextInt();

function map<A, B>(s: Rand<A>, f: (a: A) => B): Rand<B> {
    return rng => {
        const [a, rng1] = s(rng);
        const b: B = f(a);
        return [b, rng1];
    }
}

const map2 = <A, B, C>(ra: Rand<A>, rb: Rand<B>, f: (a: A, b: B) => C): Rand<C> => {
    return (rng: RNG) => {
        const [a, rng1] = ra(rng);
        const [b, rng2] = rb(rng1);
        const c: C = f(a, b);
        return [c, rng2];
    }
};

const both = <A, B>(ra: Rand<A>, rb: Rand<B>): Rand<[A, B]> => {
    return map2<A, B, [A, B]>(ra, rb, (a, b) => [a, b])
};

const flatMap = <A, B>(ra: Rand<A>, f: (a: A) => Rand<B>): Rand<B> => {
    return rng => {
        const [a, rng1] = ra(rng);
        return f(a)(rng1);
    }
};

const mapViaFlatMap = <A, B>(s: Rand<A>, f: (a: A) => B): Rand<B> => {
    return flatMap(s, a => unit(f(a)));
};

const map2ViaFlatMap = <A, B, C>(ra: Rand<A>, rb: Rand<B>, f: (a: A, b: B) => C): Rand<C> => {
    return flatMap(ra, a => mapViaFlatMap(rb, (b: B) => f(a, b)));
};

const nonNegativeInt2: Rand<number> = map(int, i => i >= 0 ? i : -(i+1));

const double2: Rand<number> = map(nonNegativeInt2, i => i/(MAX_SAFE_INTEGER + 1));

const doubleInt: Rand<[number, number]> = map2<number, number, [number, number]>(double2, int, (d, i) => [d, i]);

const doubleInt2: Rand<[number, number]> = both(double, int);

const nonNegativeIntLessThan = (n: number): Rand<number> => {
    return flatMap(nonNegativeInt2, i => {
        const mod = i % n;
        if (i + (n-1) - mod >= 0) {
            return unit(mod);
        } else {
            return nonNegativeIntLessThan(n);
        }
    });
};

console.log(map2(int, double, (a, b) => [a, b])(new SimpleRNG(1)));
console.log(map2ViaFlatMap(int, double, (a, b) => [a, b])(new SimpleRNG(1)));
