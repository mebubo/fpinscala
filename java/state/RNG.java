package state;

public interface RNG {
    Pair<Integer, RNG> nextInt();
}
