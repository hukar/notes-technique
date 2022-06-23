# 12 Formulaire sous contrôle

Pour mettre un formulaire sous le contrôle de `React`, il faut lier les `value` de ses champs au modèle de `React`, c'est à dire le `state`.

```jsx
function Form() {
    const [income, setIncome] = useState("");

    const handleIncomeChange = ({ target }) => setIncome(target.value);

    const handleSubmit = () => console.log(`income : ${income}`);

    return (
        <form onSubmit={handleSubmit}>
            <select value={income} onChange={handleIncomeChange} >
              <option value="high">high</option>
              <option value="mid">mid</option>
              <option value="low">low</option>
            </select>

            <button type="submit" className="btn btn-primary">
                Submit
            </button>
        </form>
    );
}
```

`const [income, setIncome] = useState("");` défini le modèle

`<select value={income} ...` lie le modèle au champ

`... onChange={handleIncomeChange} >` permet d'appeler le `setter` du modèle

`const handleIncomeChange = ({ target }) => setIncome(target.value);` appelle effectivement `setIncome`  pour mettre à jour le modèle