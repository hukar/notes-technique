namespace yield_keyword
{
    public class MovieDto
    {
        public string Code { get; set; } = String.Empty;
        public string Title { get; set; } = String.Empty;

        public DateTime Now { get; set; }

        public MovieDto()
        {
            Console.WriteLine("DTO is instanciated");
            Now = DateTime.Now;
        }
    }
}