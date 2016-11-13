package state;

import java.util.function.Function;

public interface Rand<A> extends Function<RNG, Pair<A, RNG>> {
    static Rand<Integer> integer() {
        return RNG::nextInt;
    }

    static <A> Rand<A> unit(A a) {
        return rng -> new Pair<>(a, rng);
    }

    static <A, B> Rand<B> flatMap(Rand<A> rand, Function<A, Rand<B>> f) {
        return rng -> {
            Pair<A, RNG> p = rand.apply(rng);
            return f.apply(p.a).apply(p.b);
        };
    }
}
