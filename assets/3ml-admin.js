var flags = sessionStorage.session || null;
var app = Elm.Main.init({flags: flags});

app.ports.elmToVega.subscribe(function(spec) {
    console.log(spec);
    window.requestAnimationFrame(function() {
        vegaEmbed("#vis", spec, {actions: false, renderer: 'svg'}).catch(console.warn);
    });
});
