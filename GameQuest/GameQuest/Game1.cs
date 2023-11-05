using FontStashSharp;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;
using Myra;
using Myra.Graphics2D.UI;

namespace GameQuest
{
    public class Game1 : Game
    {
        private GraphicsDeviceManager _graphics;
        private SpriteBatch _spriteBatch;
        private Desktop _desktop;
        private bool isOnStartMenu = true;

        public Game1()
        {
            _graphics = new GraphicsDeviceManager(this);
            Content.RootDirectory = "Content";
            IsMouseVisible = true;
        }

        protected override void Initialize()
        {
            base.Initialize();
        }

        private Widget GetStartMenu()
        {
            var panel = new Panel();

            var stack = new VerticalStackPanel();

            var positionedText = new Label();
            positionedText.HorizontalAlignment = HorizontalAlignment.Center;
            positionedText.Text = "Echoes of Elaria: The Crystals of Destiny";

            var pressEnterText = new Label();
            pressEnterText.HorizontalAlignment = HorizontalAlignment.Center;
            pressEnterText.Text = "Press Enter";

            stack.Widgets.Add(positionedText);
            stack.Widgets.Add(pressEnterText);

            panel.VerticalAlignment = VerticalAlignment.Center;
            panel.HorizontalAlignment = HorizontalAlignment.Center;

            panel.Widgets.Add(stack);

            return panel;
        }

        private Widget GetStartLoadMenu()
        {
            var panel = new Panel();

            var stack = new VerticalStackPanel();

            var startNewGame = new Button { Content = new Label { Text = "Start new game" } };
            startNewGame.HorizontalAlignment = HorizontalAlignment.Center;

            var loadGame = new Button { Content = new Label { Text = "Load game" } };
            loadGame.HorizontalAlignment = HorizontalAlignment.Center;

            stack.Widgets.Add(startNewGame);
            stack.Widgets.Add(loadGame);

            panel.VerticalAlignment = VerticalAlignment.Center;
            panel.HorizontalAlignment = HorizontalAlignment.Center;

            panel.Widgets.Add(stack);

            return panel;
        }

        protected override void LoadContent()
        {
            _spriteBatch = new SpriteBatch(GraphicsDevice);

            MyraEnvironment.Game = this;

            var startMenu = GetStartMenu();

            // Add it to the desktop
            _desktop = new Desktop();
            _desktop.Root = startMenu;
        }

        protected override void Update(GameTime gameTime)
        {
            if (GamePad.GetState(PlayerIndex.One).Buttons.Back == ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape))
                Exit();

            if (isOnStartMenu && Keyboard.GetState().IsKeyDown(Keys.Enter))
            {
                this.isOnStartMenu = false;
                _desktop.Root = GetStartLoadMenu();
            }

            base.Update(gameTime);
        }

        protected override void Draw(GameTime gameTime)
        {

            GraphicsDevice.Clear(Color.Black);
            _desktop.Render();

            base.Draw(gameTime);
        }
    }
}