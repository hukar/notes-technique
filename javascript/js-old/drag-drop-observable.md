# Exemple de *drag and drop* avec Observable

html:

```html
<div id="parent" class="alert alert-info" style="width: 250px;height: 250px;">
    <div id="widget" class="alert alert-danger" style="position:absolute;text-align:center;width:133px;" >drag me !</div>
</div>
```

js:

```js
const Observable = Rx.Observable;

const parent = document.getElementById('parent');
const widget = document.getElementById('widget');

const widgetMouseDown = Observable.fromEvent(widget, 'mousedown');
const parentMouseMove = Observable.fromEvent(parent, 'mousemove');
const parentMouseUp = Observable.fromEvent(parent, 'mouseup');

const drags = widgetMouseDown.
                map((ev) => {
                  return parentMouseMove.takeUntil(parentMouseUp)
                }).
                concatAll();

drags.forEach((ev) => {
  widget.style.left = ev.clientX + 'px';
  widget.style.top = ev.clientY + 'px';
})
```

`ObservableA.takeUntil(ObservableB)` complete l'ObservableA lorsque ObservableB émet quelque chose (jusqu'à ce que B émette).

La version complete du `forEach` contient trois **callback**.

Une erreur envoyer dans le `map` est *'catchée'* dans le `forEach`

```js
const drags = widgetMouseDown.
                map((ev) => {
                  if (ev.clientX%2 === 0) throw 'coco';
                  return parentMouseMove.takeUntil(parentMouseUp);     
                }).
                concatAll();

drags.forEach(
  function onNext(ev){
    widget.style.left = ev.clientX + 'px';
    widget.style.top = ev.clientY + 'px';
  },
  function onError(error) {
    console.log('on error : ' + error);
  },
  function onCompleted() {
    console.log('DONE!');
  }
)
```

