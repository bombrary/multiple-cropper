const outImages = ({ id, src, x, y, width, height}) => {
  const img = new Image()
  img.src = src
  img.onload = () => {
    src = clippedImageCanvas(img, x, y, width, height).toDataURL()
    app.ports.receiveClippedImage.send({ id: id, src: src });
  }
}

const clippedImageCanvas = (img, x0, y0, width, height) => {
  const canvas = document.createElement('canvas');
  canvas.width = width
  canvas.height = height
  const context = canvas.getContext('2d');
  context.drawImage(img, x0, y0, width, height, 0, 0, width, height);
  return canvas;
}

