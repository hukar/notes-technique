# 04 `Upload File`

## Création d'un composant `InputImg`

```cs
<div>
  <label>@label</label>
  <div>
  	<InputFile accept=".jpg,.jpeg,.png" OnChange="OnChange" />
  </div>
  @if(imageBase64 is not null)
    {
    <div>
        <img width="120" src="data:image/jpeg;base64,@imageBase64" />
    </div>
    }
</div>
  
@code {
  [Parameter] public string Label { get; set; } = "Image";
  private string? imageBase64;
  
  async Task OnChange(InputFileChangeEventArgs e)
  {
    var imageFile = e.GetMultipleFiles()[0];
    
    var buffer = new byte[imageFile.Size];
    await imageFile.OpenReadStream().ReadAsync(buffer);
    imageBase64 = Convert.ToBase64String(buffer);
  }
}  
```

Si on veut accepter plusieurs fichier `multiple` :

```html
<InputFile multiple accept=".jpg,.jpeg,.png" OnChange="OnChange" />
```



## Afficher un `PDF`

Avec une balise `iframe` :

```html
<div>
  <iframe src="/images/15-reunion-18_11_2021.pdf#toolbar=0" width="100%" height="500px"></iframe>
</div>
```

`#toolbar=0` empêche le téléchargement automatique.

On peut aussi utiliser le format `base 64` pour passer le `PDF` :

```html
<iframe src="data:application/pdf;base64,@imageBase64#toolbar=0" width="100%" height="240px"></iframe>
```

```cs
private string imageBase64 = Convert.ToBase64String(buffer);
```

Le `buffer` provenant ici d'un `upload` de fichier avec `InputFile`.

<img src="assets/pdf-in-iframe-base64-coded-style.png" alt="pdf-in-iframe-base64-coded-style" style="zoom:50%;" />

La limite est différente suivant le navigateur :

> ## Data URI Limits
>
> The data URI spec does not define a size limit but says applications may impose their own.
>
> - Chrome - [2MB](https://craignicol.wordpress.com/2016/07/19/excellent-export-and-the-chrome-url-limit/) for the current document. Otherwise the limit is the in-memory storage limit for arbitrary blobs: if x64 and NOT ChromeOS or Android, then 2GB; otherwise, `total_physical_memory / 5` ([source](https://stackoverflow.com/a/43816041/149428)).
> - Firefox - [unlimited](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/Data_URIs#Common_problems)
> - IE ≥ 9 & Edge - [4GB](http://caniuse.com/#feat=datauri)
> - Safari & Mobile Safari - ?
>
> https://stackoverflow.com/questions/695151/data-protocol-url-size-limitations



## Code Final

```cs
<div>
    <label>@Label</label>
    <div>
        <InputFile accept=".jpg,.jpeg,.png" OnChange="OnChange" />
    </div>
    // When you upload a new image
    @if(imageBase64 is not null)
    {
    <div>
        <img width="120" src="data:image/jpeg;base64,@imageBase64" />
    </div>
    }

		// When you have already one
    @if(!String.IsNullOrEmpty(ImageUrl))
    {
    <div>
        <img width="120" src="@ImageUrl" />
    </div>
    }
</div>



@code {
    [Parameter] public string Label { get; set; } = "Image";
    [Parameter] public EventCallback<string> OnSelectedImage { get; set; }
    // image already present with Edit Mode, can be null in Create Mode
    [Parameter] public string? ImageUrl { get; set;}
    private string? imageBase64;
    protected override void OnInitialized()
    {
        
    }

    async Task OnChange(InputFileChangeEventArgs e)
    {
        var imageFile = e.GetMultipleFiles()[0];
        var buffer = new byte[imageFile.Size];

        await imageFile.OpenReadStream(53000000).ReadAsync(buffer);
        //                            47363509


        imageBase64 = Convert.ToBase64String(buffer);

        await OnSelectedImage.InvokeAsync(imageBase64);
        // don't showing two images when upload new image
        ImageUrl = null;
    }

}
```

Dans le `EditForm`

```cs
<EditForm Model="Person" OnValidSubmit="OnValidSubmit">
    <DataAnnotationsValidator/>

    <div class="form-group">
        <InputImg Label="Hukar Img" OnSelectedImage="OnSelectedImage" ImageUrl="@imageUrl" />
    </div>

    <button class="btn btn-primary mt-3">Submit</button>

</EditForm>

@code {
    [Parameter] public Person? Person { get; set; }
    [Parameter] public EventCallback OnValidSubmit { get; set; }

    string? imageUrl;

    protected override void OnInitialized()
    {
        if(String.IsNullOrEmpty(Person!.Picture))
        {
            imageUrl = Person.Picture;
            Person.Picture = null;
        }
    }

    public void OnSelectedImage(string imageBase64)
    {
        Person!.Picture = imageBase64;
        imageUrl = null;
    }
}
```

