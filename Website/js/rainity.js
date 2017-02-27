requestAnimFrame = (function() {
return window.requestAnimationFrame ||
window.mozRequestAnimationFrame ||
window.oRequestAnimationFrame ||
window.msRequestAnimationFrame ||
window.webkitRequestAnimationFrame ||
function(callback) {
window.setTimeout(callback, 24);
};
})();

var canvas = document.getElementById('canvas');
var context = canvas.getContext('2d');

var width = 0;
var height = 0;

window.onresize = function onresize()
{
	width = canvas.width = window.innerWidth;
	height = canvas.height = window.innerHeight;
}

window.onresize();

var mouse = 
{
	X: 0,
	Y: 0
}

window.onmousemove = function onmousemove(event)
{
	mouse.X = event.clientX;
	mouse.Y = event.clientY;
}

var particles = [];
var numdef = 3;
var color = 225;
var deccolor = true;
var saturation = 100;
var lightness = 50;
var opacity = 1;
var red = 100;
var blue = 175;
var green = 100;
var alpha = 1;
var density = 1;

function addParticle(X, Y, num, a)
{
	if (!num)
	{
		num = numdef;
	}
	
	while (num--)
	{
		var vx = Math.random() * 0.25;
		var vy = Math.random() * 9 + 1;
		particles.push(
		{
			VelX: Math.random() * 0.25,
			VelY: Math.random() * 9 + 1,
			sizeX: vx * 10,
			sizeY: vx * vy * 10,
			X:X,
			Y:Y,
			alpha: a,
			color: "hsla(" + color + "," + saturation + "%, " + lightness + "%," + opacity + ")",
		})
	}
}

function update()
{
	if (color > 255)
		deccolor = true;
	if (color < 1)
		deccolor = false;
	 
	if (deccolor)
		color--;
	else
		color++;
	

	for (var i = 0, particle; particle = particles[i]; i++)
	{
		particle.X += particle.VelX;
		particle.Y += particle.VelY+5;
		
		if (particle.Y > height - 8)
		{
			particles.splice(i--, 1);
		}
	}
	
	var i = density;
	while (i--)
	{
		addParticle(Math.floor(Math.random() * width), -15, density, 1);
	}
}

function render(context)
{
	context.save();
	context.fillStyle = 'rgba(' + 0 + ',' + 0 + ',' + 0 + ',' + 255 + ')'; 
	context.fillRect(0, 0, width, height);
	
	for (var i = 0, particle; particle = particles[i]; i++)
	{
		context.globalAlpha = particle.alpha;
		context.fillStyle = particle.color;
		context.fillRect(particle.X, particle.Y, particle.sizeX, particle.sizeY);
	}
	
	context.restore();
}

(function head()
{
	requestAnimFrame(head);
	update();
	render(context);
})();























