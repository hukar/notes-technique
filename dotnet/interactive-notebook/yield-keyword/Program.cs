var db = new MoviesRepository();

var movies = db.GetAllMovies();

var moviesDto = MapMovies(movies);

foreach (var movieDto in moviesDto)
{
    Console.WriteLine($"{movieDto.Code} - {movieDto.Title} - {movieDto.Now}");
}

IEnumerable<MovieDto> MapMovies(List<Movie> movies)
{
    var moviesDto = new List<MovieDto>();
    var movieDto = new MovieDto();

    foreach (var movie in movies)
    {
        movieDto.Code = movie.Code;
        movieDto.Title = movie.Title;

        yield return movieDto;
    }
}