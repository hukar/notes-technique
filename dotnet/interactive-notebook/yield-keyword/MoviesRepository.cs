namespace yield_keyword;
public class MoviesRepository
{
    
    private static Random rand = new();
    private List<Movie> movies = new() {
        new() {
            Id = 1,
            Title = "X-Men",
            Code = $"{rand.Next(1,99)}{rand.Next(1,99)}{rand.Next(1,99)}-{rand.Next(1,99)}"
        },
        new() {
            Id = 2,
            Title = "Spider-Man 2",
            Code = $"{rand.Next(1,99)}{rand.Next(1,99)}{rand.Next(1,99)}-{rand.Next(1,99)}"
        },
        new() {
            Id = 3,
            Title = "Hulk",
            Code = $"{rand.Next(1,99)}{rand.Next(1,99)}{rand.Next(1,99)}-{rand.Next(1,99)}"
        },
    };

    public List<Movie> GetAllMovies() => movies;
}
