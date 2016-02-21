package whatever;

/**
 * @author avhaliullin
 */
public final class Unit {
    private Unit() {
    }

    @Override
    public final String toString() {
        return "()";
    }

    public static final Unit INSTANCE = new Unit();
}
