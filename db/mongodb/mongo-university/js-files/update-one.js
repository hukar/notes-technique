let filter = { title: "Star Wars: Episode III - Revenge of the Sith" };

let doc = db.movieDetails.findOne(filter);

doc.type;
doc.type = "genius";

doc.genres;
doc.genres.push("TV Series");

db.movieDetails.replaceOne(filter, doc);