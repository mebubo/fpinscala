package state;

public class SimpleRNG implements RNG {

    private Long seed;

    public SimpleRNG(Long seed) {
        this.seed = seed;
    }

    @Override
    public Pair<Integer, RNG> nextInt() {
        long newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL;
        RNG nextRNG = new SimpleRNG(newSeed);
        int n = (int) (newSeed >>> 16);
        return new Pair<>(n, nextRNG);
    }
}
