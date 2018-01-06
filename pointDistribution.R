#Converts spherical coordinates to cartesian coordinates
sphere_to_cart <- function(r, theta, psi) {
  c(r*sin(theta)*cos(psi), r*sin(theta)*sin(psi), r*cos(theta));
}

#Returns the euclidian distance between two spherical coordinates
sphere_squared_dist <- function(r, theta1, psi1, theta2, psi2) {
  x = sphere_to_cart(r, theta1, psi1);
  y = sphere_to_cart(r, theta2, psi2);
  cart_squared_dist(x, y);
}

#Takes two vectors, computes squared distance
cart_squared_dist <- function(x1, x2) {
  (x1[1]-x2[1])**2 + (x1[2]-x2[2])**2 + (x1[3]-x2[3])**2
}


#calculates partial derivative of distance between two spherical coordinates relative to theta of the first point
partialD_theta_i <- function(r, theta1, psi1, theta2, psi2) {
  2 * ( r*( sin(theta1)*cos(psi1) - sin(theta2)*cos(psi2) ) * r*cos(theta1)*cos(psi1) +
        r*( sin(theta1)*sin(psi1) - sin(theta2)*sin(psi2) ) * r*cos(theta1)*sin(psi1) -
        (r*( cos(theta1) - cos(theta2) ) * r*sin(theta1))
      );
}


#radius
r <- 4;
delta_theta = .1;

theta_i = pi / 4.2;
psi_i = pi / 4;

theta_j = pi / 4;
psi_j = pi / 4;

dist_before = sphere_squared_dist(r, theta_i, psi_i, theta_j, psi_j);
rate_of_change = partialD_theta_i(r, theta_i, psi_i, theta_j, psi_j);

expected_dist_after = dist_before + rate_of_change*delta_theta;

theta_i = theta_i + delta_theta;
actual_dist_after = sphere_squared_dist(r, theta_i, psi_i, theta_j, psi_j);


